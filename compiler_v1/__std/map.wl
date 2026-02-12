// std/map.wl
//
// Standard Map Library.
// This files provides the definition and operations of maps.

// ----------------------------------------------------------
// C Runtime Bindings
// ----------------------------------------------------------
extern "C" {
    func calloc(num -> Long, size -> Long) -> String;
    
    // Standard memory allocation
    func malloc(size -> Long) -> String;
    
    // Releases memory
    func free(p -> String) -> Void;
    
    // Compares two strings. Returns 0 if they are equal.
    func strcmp(s1 -> String, s2 -> String) -> Int;
    
    // Returns the length of the string
    func strlen(s -> String) -> Long;
}

// ==========================================================
// Type Definitions
// ==========================================================
struct MapEntry(
    key   -> String, 
    value -> Struct, 
    next  -> MapEntry
)

struct HashMap(
    buckets  -> ptr MapEntry, 
    capacity -> Int,          
    size     -> Int           
)

// ----------------------------------------------------------
// Hash Function (DJB2)
// ----------------------------------------------------------
func hash_djb2(key -> String) -> Int {
    let hash -> Long = 5381;
    let i -> Int = 0;
    
    while (i < key.length) {
        let c -> Int = key[i];
        if (c == 0) { break; }
        hash = ((hash * 33) + c);
        i++;
    }
    
    if (hash < 0) {
        hash = 0 - hash;
    }

    let final_hash -> Int = hash;
    if (final_hash < 0) {
        final_hash = 0 - final_hash; // absolute value
    }
    return final_hash;
}


// ==========================================================
// Map Operations
// ==========================================================
func map_new(capacity -> Int) -> HashMap {
    let raw_mem -> String = calloc(capacity, 8);
    let ptr buckets -> MapEntry = raw_mem;

    return HashMap(buckets=buckets, capacity=capacity, size=0);
}


func map_put(self -> HashMap, key -> String, val -> Struct) -> Void {
    let h -> Int = hash_djb2(key);
    let idx -> Int = h % self.capacity;
    if (idx < 0) { idx = 0 - idx; }
    let current -> MapEntry = self.buckets[idx];
    let iter -> MapEntry = current;
    while (iter != null) {
        if (strcmp(iter.key, key) == 0) {
            iter.value = val;
            return;
        }
        iter = iter.next;
    }
    
    let new_entry -> MapEntry = MapEntry(key=key, value=val, next=current);
    self.buckets[idx] = new_entry;
    
    self.size++;
}


func map_get(self -> HashMap, key -> String) -> Struct {
    let h -> Int = hash_djb2(key);
    let idx -> Int = h % self.capacity;
    
    // Retrieve the head of the chain
    let current -> MapEntry = self.buckets[idx];
    
    // Traverse the chain
    while (current != null) {
        if (strcmp(current.key, key) == 0) {
            return current.value;
        }
        current = current.next;
    }
    
    return null; // Not found
}

func map_free(self -> HashMap) -> Void {
    let raw_buckets -> String = self.buckets;
    free(raw_buckets);
}

// in keyword support
func _compiler_helper_IN(self -> HashMap, key -> String) -> Bool {
    let val -> Struct = map_get(self, key);
    return val != null;
}


