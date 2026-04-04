// std/map.wl
extern "C" {
    func calloc(num -> Long, size -> Long) -> ptr Void;
    func free(ptr p -> Void) -> Void;
    func strcmp(s1 -> String, s2 -> String) -> Int;
}


func hash_djb2(key -> String) -> Int {
    let hash -> Int = 5381;
    let i -> Int = 0;
    while (i < key.length()) {
        let c -> Int = key[i];
        if (c == 0) { break; }
        hash = ((hash * 33) + c);
        i++;
    }
    if (hash < 0) { hash = 0 - hash; }
    return hash;
}


class MapEntry {
    let key   -> String = "";
    let value -> Struct = null;
    let next  -> MapEntry = null;

    init(k -> String, v -> Struct, n -> MapEntry) {
        self.key = k;
        self.value = v;
        self.next = n;
    }
}


class HashMap {
    let ptr buckets -> MapEntry = nullptr; 
    let capacity -> Int = 0;          
    let size     -> Int = 0;           

    init(cap -> Int) {
        self.capacity = cap;
        self.size = 0;
        self.buckets = calloc(cap, 8);
    }

    method put(key -> String, val -> Struct) -> Void {
        let h -> Int = hash_djb2(key);
        let idx -> Int = h % self.capacity;
        if (idx < 0) { idx = 0 - idx; }
        
        let current -> MapEntry = self.buckets[idx];
        let iter -> MapEntry = current;
        
        while (iter is !null) {
            if (strcmp(iter.key, key) == 0) {
                iter.value = val;
                return;
            }
            iter = iter.next;
        }
        
        self.buckets[idx] = MapEntry(key, val, current);
        self.size++;
    }

    method get(key -> String) -> Struct {
        let h -> Int = hash_djb2(key);
        let idx -> Int = h % self.capacity;
        
        let current -> MapEntry = self.buckets[idx];
        while (current is !null) {
            if (strcmp(current.key, key) == 0) {
                return current.value;
            }
            current = current.next;
        }
        return null;
    }

    deinit() {
        if (self.buckets is !nullptr) {
            free(self.buckets);
            self.buckets = nullptr;
        }
    }
}

func _compiler_helper_IN(self -> HashMap, key -> String) -> Bool {
    let val -> Struct = self.get(key);
    return val is !null;
}
