import Error from "errors"
import "internal/runtime/string" as runtime_string

class Buffer {
    let __storage -> String = null;
    let __length -> Int = 0;
    let __capacity -> Int = 0;

    init(initial_capacity -> Int) {
        if (initial_capacity < 64) { initial_capacity = 64; }
        self.__storage = runtime_string.alloc(Long(initial_capacity));
        if (self.__storage is !null) {
            runtime_string.set_length(self.__storage, 0);
            self.__capacity = initial_capacity;
        }
    }

    method __reserve(additional -> Int) -> Void? {
    // grow geometrically to keep repeated appends linear
        if (additional < 0 || self.__length > 2147483647 - additional) {
            throw Error.Overflow;
        }
        let required -> Int = self.__length + additional;
        if (required <= self.__capacity) { return; }

        let capacity -> Int = self.__capacity;
        if (capacity < 64) { capacity = 64; }
        while (capacity < required) {
            if (capacity > 1073741823) {
                capacity = required;
                break;
            }
            capacity *= 2;
        }

        let replacement -> String = runtime_string.alloc(Long(capacity));
        if (replacement is null) { throw Error.OutOfMemory; }
        let ptr source -> Byte = runtime_string.data(self.__storage);
        let ptr target -> Byte = runtime_string.data(replacement);
        let i -> Int = 0;
        while (i < self.__length) {
            target[i] = source[i];
            i += 1;
        }
        runtime_string.set_length(replacement, self.__length);
        self.__storage = replacement;
        self.__capacity = capacity;
        return;
    }

    method append_byte(value -> Byte) -> Void? {
        self.__reserve(1)?;
        let ptr bytes -> Byte = runtime_string.data(self.__storage);
        bytes[self.__length] = value;
        self.__length += 1;
        runtime_string.set_length(self.__storage, self.__length);
        return;
    }

    method append(value -> String) -> Void? {
        if (value is null) { throw Error.InvalidArgument; }
        self.__reserve(value.length())?;
        let ptr source -> Byte = runtime_string.data(value);
        let ptr target -> Byte = runtime_string.data(self.__storage);
        let i -> Int = 0;
        while (i < value.length()) {
            target[self.__length + i] = source[i];
            i += 1;
        }
        self.__length += value.length();
        runtime_string.set_length(self.__storage, self.__length);
        return;
    }

    method append_char(value -> Char) -> Void? {
        let encoded -> String = runtime_string.encode_utf8_char(value);
        if (encoded is null) { throw Error.InvalidArgument; }
        self.append(encoded)?;
        return;
    }

    method length() -> Int {
        return self.__length;
    }

    method finish() -> String? {
        if (self.__storage is null) { throw Error.OutOfMemory; }
        runtime_string.set_length(self.__storage, self.__length);
        return self.__storage;
    }
}
