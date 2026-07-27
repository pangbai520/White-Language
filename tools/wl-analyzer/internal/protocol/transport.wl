// framed standard I/O transport
import "io"
import Error from "errors"

func __header_length(line -> String) -> Int {
    let prefix -> String = "Content-Length:";
    if (!line.starts_with(prefix)) { return -1; }
    let i -> Int = prefix.length();
    while (i < line.length() && (line[i] == ' ' || line[i] == '\t')) { i += 1; }

    let length -> Int = 0;
    let found -> Bool = false;
    while (i < line.length() && line[i] >= '0' && line[i] <= '9') {
        found = true;
        length = length * 10 + Int(line[i]) - Int('0');
        i += 1;
    }
    if (!found) { return -1; }
    return length;
}

func read_message() -> String? {
    // return null only when the input stream has closed between messages
    let content_length -> Int = -1;
    while true {
        let line -> String = io.stdin.read_line()?;
        catch(err) {
            return "";
        }
        if (line.length() == 0) { break; }
        let parsed -> Int = __header_length(line);
        if (parsed >= 0) { content_length = parsed; }
    }
    if (content_length < 0) { throw Error.InvalidArgument; }
    return io.stdin.read_full(content_length)?;
}

func write_message(body -> String) -> Void? {
    io.stdout.write_all("Content-Length: " + body.length() + "\r\n\r\n")?;
    io.stdout.write_all(body)?;
    io.stdout.flush()?;
    return;
}
