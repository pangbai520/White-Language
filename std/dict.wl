// std/map.wl
// Basic open-addressed hash map. Uses linear probing and "tombstones" for deletes.
// Forced 2^n capacity so we can use bitmasking instead of slow modulo ops.

extern "C" {
    func calloc(num -> Long, size -> Long) -> ptr Void;
    func free(ptr p -> Void) -> Void;
    func strcmp(s1 -> String, s2 -> String) -> Int;
}


func hash_string(key -> String) -> Int {
    let hash -> Int = 5381;
    let i -> Int = 0;
    let len -> Int = key.length();
    
    while (i < len) {
        let c -> Int = key[i];
        if (c == 0) { break; }
        hash = ((hash << 5) + hash) ^ c;
        i++;
    }

    if (hash < 0) { hash = ~hash; }

    if (hash < 2) { hash += 2; }
    
    return hash;
}

class Dict {
    let ptr keys      -> String = nullptr; 
    let ptr values    -> Struct = nullptr; 
    // this array stores the hashes but also tracks state (0 = empty, 1 = deleted)
    let ptr hashes    -> Int    = nullptr; 
    
    let capacity      -> Int = 0;
    let size          -> Int = 0;
    let tombstones    -> Int = 0;

    init(cap -> Int) {
        // minimum size of 8, then find the next power of 2
        let actual_cap -> Int = 8;
        while (actual_cap < cap) {
            actual_cap <<= 1;
        }
        self.capacity = actual_cap;
        self.size = 0;
        self.tombstones = 0;
        
        // 8 bytes for pointers, 4 for the hash ints
        self.keys   = calloc(actual_cap, 8); 
        self.values = calloc(actual_cap, 8); 
        self.hashes = calloc(actual_cap, 4); 
    }

    method _resize() -> Void {
        let old_cap -> Int = self.capacity;
        let ptr old_keys -> String = self.keys;
        let ptr old_vals -> Struct = self.values;
        let ptr old_hashes -> Int  = self.hashes;

        // standard x2 growth strategy
        self.capacity <<= 1; 
        self.keys   = calloc(self.capacity, 8);
        self.values = calloc(self.capacity, 8);
        self.hashes = calloc(self.capacity, 4);
        
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

        free(old_keys);
        free(old_vals);
        free(old_hashes);
    }

    method put(key -> String, val -> Struct) -> Void {
        // if we're 75% full (counting dead slots), we need to grow
        if ((self.size + self.tombstones) * 3 >= self.capacity << 1) {
            self._resize();
        }

        let hash -> Int = hash_string(key);
        // fast bitwise AND for index, only works because capacity is a power of 2.
        let mask -> Int = self.capacity - 1;
        let idx  -> Int = hash & mask;
        let first_tombstone -> Int = -1;

        while (true) {
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

    method get(key -> String) -> Struct {
        if (self.size == 0) { return null; }

        let hash -> Int = hash_string(key);
        let mask -> Int = self.capacity - 1;
        let idx  -> Int = hash & mask;

        while (true) {
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

        while (true) {
            let curr_h -> Int = self.hashes[idx];
            
            if (curr_h == 0) { return; } // nothing to delete
            
            if (curr_h == hash) {
                if (self.keys[idx] == key) {
                    // turn this into a "tombstone" so we don't break the probe chain
                    self.hashes[idx] = 1; 
                    self.keys[idx] = "";  // clear the ref
                    self.values[idx] = null;
                    self.size--;
                    self.tombstones++;
                    return;
                }
            }
            idx = (idx + 1) & mask;
        }
    }

    deinit() {
        // standard cleanup to avoid leaking memory
        if (self.keys is !nullptr) {
            free(self.keys);
            self.keys = nullptr;
        }
        if (self.values is !nullptr) {
            free(self.values);
            self.values = nullptr;
        }
        if (self.hashes is !nullptr) {
            free(self.hashes);
            self.hashes = nullptr;
        }
    }
}

// internal hook for the compiler's 'in' operator
func _compiler_helper_IN(self -> Dict, key -> String) -> Bool {
    let val -> Struct = self.get(key);
    return val is !null;
}