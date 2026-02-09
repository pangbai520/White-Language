// std/map.wl
//
// Standard Hash Map Library.
// This module provides a high-performance key-value store using a hash table.
// It uses separate chaining for collision resolution and the DJB2 hash algorithm.
//
// Key Features:
//   - Generic Storage: Uses 'Struct' (void*) to store any object reference
//   - Efficient Hashing: Implementation of the DJB2 algorithm
//   - Dynamic: Supports standard put/get/contains operations
//   - Native: Direct memory manipulation using WhiteLang pointer arithmetic

// ----------------------------------------------------------
// C Runtime Bindings
// ----------------------------------------------------------
extern "C" {
    // Allocates memory for an array of num elements of size bytes each
    // and initializes all bytes to zero. Critical for empty buckets.
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

// Represents a single key-value pair in the map.
// If a hash collision occurs, 'next' points to the next entry in the bucket.
// value: Type 'Struct' represents a generic pointer (i8*), 
//        allowing storage of any struct reference.
struct MapEntry(
    key   -> String, 
    value -> Struct, 
    next  -> MapEntry
)

// The main Hash Map structure.
// buckets: A pointer to an array of MapEntry pointers (MapEntry**).
//          In WhiteLang, 'ptr MapEntry' creates a pointer to the struct reference.
struct HashMap(
    buckets  -> ptr MapEntry, 
    capacity -> Int,          
    size     -> Int           
)


// ==========================================================
// Internal Helper Functions
// ==========================================================

// ----------------------------------------------------------
// Hash Function (DJB2)
// ----------------------------------------------------------
// A classic, fast hash function for strings.
// Returns a positive integer hash code.
func hash_djb2(key -> String) -> Int {
    let hash -> Long = 5381;
    let i -> Int = 0;
    
    while (i < key.length) {
        // Implicit cast: Byte -> Int
        // Accesses the i-th byte of the string
        let c -> Int = key[i];
        
        if (c == 0) { break; }
        
        // hash = ((hash << 5) + hash) + c
        // Compiler promotes 'c' to Long automatically
        hash = ((hash * 33) + c);
        i++;
    }
    
    // Ensure positive result for modulo operations
    if (hash < 0) {
        hash = 0 - hash;
    }

    // Implicit cast: Long -> Int (Truncation)
    let final_hash -> Int = hash;
    return final_hash;
}


// ==========================================================
// Map Operations
// ==========================================================

// ----------------------------------------------------------
// Create Map
// ----------------------------------------------------------
// Creates a new HashMap with the specified initial capacity.
// Capacity should ideally be a power of 2 (e.g., 16, 64).
func map_new(capacity -> Int) -> HashMap {
    // 1. Allocate bucket array
    // We need 'capacity' number of pointers.
    // Each pointer (MapEntry) is 8 bytes on 64-bit systems.
    // calloc returns String (i8*) initialized to zero (null pointers).
    let raw_mem -> String = calloc(capacity, 8);
    
    // 2. Cast generic memory to typed pointer array
    // String (i8*) -> ptr MapEntry (MapEntry**)
    // This relies on the compiler's implicit bitcast logic.
    let ptr buckets -> MapEntry = raw_mem;

    return HashMap(buckets=buckets, capacity=capacity, size=0);
}


// ----------------------------------------------------------
// Put (Insert/Update)
// ----------------------------------------------------------
// Associates the specified value with the specified key.
// If the key exists, updates the value.
// If the key is new, inserts it at the head of the collision chain.
func map_put(self -> HashMap, key -> String, val -> Struct) -> Void {
    let h -> Int = hash_djb2(key);
    let idx -> Int = h % self.capacity;
    
    // Direct Pointer Indexing!
    // self.buckets is `ptr MapEntry` (Struct**)
    // self.buckets[idx] generates a GEP and Loads the `MapEntry` (Struct*) at that index.
    let current -> MapEntry = self.buckets[idx];
    
    // 1. Check for existing key (Update)
    let iter -> MapEntry = current;
    while (iter != null) {
        if (strcmp(iter.key, key) == 0) {
            iter.value = val; // Update existing value
            return;
        }
        iter = iter.next;
    }
    
    // 2. Insert new entry (Head Insert)
    // Create a new node pointing to the current head
    let new_entry -> MapEntry = MapEntry(key=key, value=val, next=current);
    
    // Update the bucket pointer to point to the new entry
    // Equivalent to: buckets[idx] = new_entry
    self.buckets[idx] = new_entry;
    
    self.size++;
}


// ----------------------------------------------------------
// Contains Key
// ----------------------------------------------------------
// Returns true if the map contains a mapping for the specified key.
func _compiler_helper_IN(self -> HashMap, key -> String) -> Bool {
    // Re-use Map_get logic
    let val -> Struct = map_get(self, key);
    return val != null;
}

// ----------------------------------------------------------
// Get (Retrieve)
// ----------------------------------------------------------
// Returns the value mapped to the key, or null if not found.
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


// ----------------------------------------------------------
// Free Map (Optional)
// ----------------------------------------------------------
// Releases the memory used by the map structure.
// Note: This does NOT free the values stored in the map, 
// as the map does not own the objects.
func map_free(self -> HashMap) -> Void {
    // Free the bucket array
    // Cast ptr MapEntry -> String (i8*) for free()
    let raw_buckets -> String = self.buckets;
    free(raw_buckets);
    
    // Note: To be fully correct, we should iterate and free each MapEntry node too.
    // For compiler bootstrapping, we often rely on OS cleanup at exit, 
    // but implementing full cleanup is a good exercise.
}