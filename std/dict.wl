// std/dict.wl
// Basic open-addressed hash map using linear probing and tombstones
// capacities stay at powers of two so probing can use a bitmask

import "internal/runtime"

@CompilerIntrinsic
struct Variant(
    // compiler internal implementation
)

func hash_string(key -> String) -> Int {
    let hash -> Int = 5381;
    let i -> Int = 0;
    let len -> Int = key.length();

    while (i < len) {
        let c -> Char = key[i];
        if (c == '\0') { break; }
        hash = ((hash << 5) + hash) ^ Int(c);
        i++;
    }

    if (hash < 0) { hash = ~hash; }
    if (hash < 2) { hash += 2; }

    return hash;
}

class Dict {
    let ptr keys      -> String = nullptr; 
    let ptr values    -> Variant = nullptr; 
    // this array stores the hashes but also tracks state (0 = empty, 1 = deleted)
    let ptr hashes    -> Int    = nullptr; 

    let capacity      -> Int = 0;
    let size          -> Int = 0;
    let tombstones    -> Int = 0;

    init(cap -> Int) {
        // minimum size of 8, then find the next power of 2
        let actual_cap -> Int = 8;
        while (actual_cap < cap) {
            if (actual_cap >= 1073741824) {
                runtime.process_exit(1);
                return;
            }
            actual_cap <<= 1;
        }

        // keys and variants are stored as pointers, hashes are 32-bit values
        let ptr new_keys -> String = runtime.mem_alloc_zeroed(Long(actual_cap) * 8L);
        let ptr new_values -> Variant = runtime.mem_alloc_zeroed(Long(actual_cap) * 8L);
        let ptr new_hashes -> Int = runtime.mem_alloc_zeroed(Long(actual_cap) * 4L);
        if (new_keys is nullptr || new_values is nullptr || new_hashes is nullptr) {
            runtime.mem_dealloc(new_keys);
            runtime.mem_dealloc(new_values);
            runtime.mem_dealloc(new_hashes);
            runtime.process_exit(1);
            return;
        }

        self.keys = new_keys;
        self.values = new_values;
        self.hashes = new_hashes;
        self.capacity = actual_cap;
        self.size = 0;
        self.tombstones = 0;
    }

    method __release_slots(ptr keys -> String, ptr values -> Variant, ptr hashes -> Int, cap -> Int) -> Void {
        let i -> Int = 0;
        while (i < cap) {
            if (hashes[i] >= 2) {
                keys[i] = null;
                values[i] = null;
            }
            i += 1;
        }
    }

    method __resize() -> Void {
        let old_cap -> Int = self.capacity;
        let ptr old_keys -> String   = self.keys;
        let ptr old_vals -> Variant = self.values;
        let ptr old_hashes -> Int    = self.hashes;

        // standard x2 growth strategy
        if (old_cap >= 1073741824) {
            runtime.process_exit(1);
            return;
        }
        let new_cap -> Int = old_cap << 1;
        let ptr new_keys -> String = runtime.mem_alloc_zeroed(Long(new_cap) * 8L);
        let ptr new_vals -> Variant = runtime.mem_alloc_zeroed(Long(new_cap) * 8L);
        let ptr new_hashes -> Int = runtime.mem_alloc_zeroed(Long(new_cap) * 4L);
        if (new_keys is nullptr || new_vals is nullptr || new_hashes is nullptr) {
            runtime.mem_dealloc(new_keys);
            runtime.mem_dealloc(new_vals);
            runtime.mem_dealloc(new_hashes);
            runtime.process_exit(1);
            return;
        }

        self.capacity = new_cap;
        self.keys = new_keys;
        self.values = new_vals;
        self.hashes = new_hashes;

        self.size = 0;
        self.tombstones = 0; 

        let i -> Int = 0;
        while (i < old_cap) {
            // only migrate slots that actually have data (hash >= 2)
            if (old_hashes[i] >= 2) {
                self.put(old_keys[i], old_vals[i]);
            }
            i++;
        }

        self.__release_slots(old_keys, old_vals, old_hashes, old_cap);
        runtime.mem_dealloc(old_keys);
        runtime.mem_dealloc(old_vals);
        runtime.mem_dealloc(old_hashes);
    }

    method put(key -> String, val -> Variant) -> Void {
        // if we're 75% full (counting dead slots), we need to grow
        if ((self.size + self.tombstones) * 3 >= self.capacity << 1) {
            self.__resize();
        }

        let hash -> Int = hash_string(key);
        // fast bitwise AND for index, only works because capacity is a power of 2.
        let mask -> Int = self.capacity - 1;
        let idx  -> Int = hash & mask;
        let first_tombstone -> Int = -1;

        while true {
            let curr_h -> Int = self.hashes[idx];

            // found a fresh spot
            if (curr_h == 0) { 
                // if we passed a tombstone earlier, reuse that slot instead of wasting space
                if (first_tombstone != -1) {
                    idx = first_tombstone;
                    self.tombstones--;
                }
                self.hashes[idx] = hash;
                self.keys[idx] = key;
                self.values[idx] = val;
                self.size++;
                return;
            }

            // keep track of the first dead slot we see in case we need to insert
            if (curr_h == 1) { 
                if (first_tombstone == -1) {
                    first_tombstone = idx; 
                }
            } 
            // check the hash first before doing a heavy string comparison
            else if (curr_h == hash) {
                if (self.keys[idx] == key) { 
                    self.values[idx] = val; // key already exists, just update the value
                    return;
                }
            }

            // collision or tombstone: keep walking
            idx = (idx + 1) & mask;
        }
    }

    method get(key -> String) -> Variant {
        if (self.size == 0) { return null; } // 0 means empty

        let hash -> Int = hash_string(key);
        let mask -> Int = self.capacity - 1;
        let idx  -> Int = hash & mask;

        while true {
            let curr_h -> Int = self.hashes[idx];

            // if we hit an empty slot, the key definitely isn't here
            if (curr_h == 0) { 
                return null; 
            }

            // quick hash check, then confirm with a proper string compare
            if (curr_h == hash) {
                if (self.keys[idx] == key) {
                    return self.values[idx];
                }
            }

            // don't stop on tombstones (1), the item might be further down the line
            idx = (idx + 1) & mask;
        }
        return null;
    }

    method remove(key -> String) -> Void {
        if (self.size == 0) { return; }

        let hash -> Int = hash_string(key);
        let mask -> Int = self.capacity - 1;
        let idx  -> Int = hash & mask;

        while true {
            let curr_h -> Int = self.hashes[idx];

            if (curr_h == 0) { return; } // nothing to delete

            if (curr_h == hash) {
                if (self.keys[idx] == key) {
                    // turn this into a "tombstone" so we don't break the probe chain
                    self.hashes[idx] = 1; 
                    self.keys[idx] = null;
                    self.values[idx] = null;
                    self.size--;
                    self.tombstones++;
                    return;
                }
            }
            idx = (idx + 1) & mask;
        }
    }

    method contains_key(key -> String) -> Bool {
        if (self.size == 0) { return false; }

        let hash -> Int = hash_string(key);
        let mask -> Int = self.capacity - 1;
        let idx  -> Int = hash & mask;

        while true {
            let curr_h -> Int = self.hashes[idx];
            if (curr_h == 0) { 
                return false; 
            }

            if (curr_h == hash) {
                if (self.keys[idx] == key) {
                    return true;
                }
            }

            idx = (idx + 1) & mask;
        }
        return false;
    }

    deinit() {
        // standard cleanup to avoid leaking memory
        if (self.keys is !nullptr && self.values is !nullptr && self.hashes is !nullptr) {
            self.__release_slots(self.keys, self.values, self.hashes, self.capacity);
        }
        if (self.keys is !nullptr) {
            runtime.mem_dealloc(self.keys);
            self.keys = nullptr;
        }
        if (self.values is !nullptr) {
            runtime.mem_dealloc(self.values);
            self.values = nullptr;
        }
        if (self.hashes is !nullptr) {
            runtime.mem_dealloc(self.hashes);
            self.hashes = nullptr;
        }
    }
}
