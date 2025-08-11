module LogLevels

let message (logLine: string): string = 
    let parts = logLine.Split([|':'|])
    parts[1].Trim()

let logLevel(logLine: string): string = 
    let parts = logLine.Split([|'['; ']'|])
    parts[1].ToLower()
    

let reformat(logLine: string): string = 
    message logLine + " (" + logLevel logLine + ")"
