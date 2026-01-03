// Azimuth Cross-Platform Compatibility Tests
// This file contains comprehensive test cases for cross-platform compatibility

// Test 1: Path Handling Across Platforms
test "path handling across different platforms" {
  type PathType {
    Windows
    Unix
    Mac
  }
  
  type PathInfo {
    type : PathType
    separator : String
    root_prefix : String
  }
  
  let get_path_info = fn(path_type : PathType) : PathInfo {
    match path_type {
      PathType::Windows => {
        {
          type: PathType::Windows,
          separator: "\\",
          root_prefix: "C:\\\\"
        }
      }
      PathType::Unix => {
        {
          type: PathType::Unix,
          separator: "/",
          root_prefix: "/"
        }
      }
      PathType::Mac => {
        {
          type: PathType::Mac,
          separator: "/",
          root_prefix: "/"
        }
      }
    }
  }
  
  let normalize_path = fn(path : String, path_type : PathType) : String {
    let info = get_path_info(path_type)
    let mut normalized = ""
    
    // Replace all separators with the platform-specific one
    for i in 0..<path.length() {
      let char = path.substring(i, 1)
      if char == "/" || char == "\\" {
        normalized = normalized + info.separator
      } else {
        normalized = normalized + char
      }
    }
    
    // Remove duplicate separators
    let mut result = ""
    let mut prev_was_separator = false
    
    for i in 0..<normalized.length() {
      let char = normalized.substring(i, 1)
      if char == info.separator {
        if !prev_was_separator {
          result = result + char
          prev_was_separator = true
        }
      } else {
        result = result + char
        prev_was_separator = false
      }
    }
    
    result
  }
  
  let join_paths = fn(paths : Array[String], path_type : PathType) : String {
    if paths.length() == 0 {
      return ""
    }
    
    let info = get_path_info(path_type)
    let mut result = paths[0]
    
    for i in 1..<paths.length() {
      // Remove trailing separator from current result
      if result.length() > 0 && result.substring(result.length() - 1, 1) == info.separator {
        result = result.substring(0, result.length() - 1)
      }
      
      // Remove leading separator from next path
      let mut next_path = paths[i]
      if next_path.length() > 0 && next_path.substring(0, 1) == info.separator {
        next_path = next_path.substring(1, next_path.length() - 1)
      }
      
      result = result + info.separator + next_path
    }
    
    result
  }
  
  let get_file_extension = fn(path : String) : String {
    let last_dot_pos = path.length() - 1
    let mut dot_pos = -1
    
    for i in 0..<path.length() {
      if path.substring(i, 1) == "." {
        dot_pos = i
      }
    }
    
    if dot_pos >= 0 && dot_pos < last_dot_pos {
      path.substring(dot_pos + 1, path.length() - dot_pos - 1)
    } else {
      ""
    }
  }
  
  let get_file_name = fn(path : String, path_type : PathType) : String {
    let info = get_path_info(path_type)
    let normalized = normalize_path(path, path_type)
    let mut last_separator_pos = -1
    
    for i in 0..<normalized.length() {
      if normalized.substring(i, 1) == info.separator {
        last_separator_pos = i
      }
    }
    
    if last_separator_pos >= 0 {
      normalized.substring(last_separator_pos + 1, normalized.length() - last_separator_pos - 1)
    } else {
      normalized
    }
  }
  
  // Test path normalization
  assert_eq(normalize_path("folder/subfolder/file.txt", PathType::Unix), "folder/subfolder/file.txt")
  assert_eq(normalize_path("folder\\subfolder\\file.txt", PathType::Unix), "folder/subfolder/file.txt")
  assert_eq(normalize_path("folder//subfolder/file.txt", PathType::Unix), "folder/subfolder/file.txt")
  
  assert_eq(normalize_path("folder\\subfolder\\file.txt", PathType::Windows), "folder\\subfolder\\file.txt")
  assert_eq(normalize_path("folder/subfolder/file.txt", PathType::Windows), "folder\\subfolder\\file.txt")
  assert_eq(normalize_path("folder\\\\subfolder\\file.txt", PathType::Windows), "folder\\subfolder\\file.txt")
  
  // Test path joining
  let unix_paths = ["home", "user", "documents", "file.txt"]
  assert_eq(join_paths(unix_paths, PathType::Unix), "home/user/documents/file.txt")
  
  let windows_paths = ["C:", "Users", "Documents", "file.txt"]
  assert_eq(join_paths(windows_paths, PathType::Windows), "C:\\\\Users\\\\Documents\\\\file.txt")
  
  // Test with empty paths
  let mixed_paths = ["home", "", "user", "documents", "", "file.txt"]
  assert_eq(join_paths(mixed_paths, PathType::Unix), "home/user/documents/file.txt")
  
  // Test file extension extraction
  assert_eq(get_file_extension("file.txt"), "txt")
  assert_eq(get_file_extension("document.pdf"), "pdf")
  assert_eq(get_file_extension("archive.tar.gz"), "gz")
  assert_eq(get_file_extension("noextension"), "")
  assert_eq(get_file_extension(".hiddenfile"), "")
  
  // Test file name extraction
  assert_eq(get_file_name("folder/subfolder/file.txt", PathType::Unix), "file.txt")
  assert_eq(get_file_name("folder\\subfolder\\file.txt", PathType::Windows), "file.txt")
  assert_eq(get_file_name("file.txt", PathType::Unix), "file.txt")
  assert_eq(get_file_name("/home/user/file.txt", PathType::Unix), "file.txt")
  assert_eq(get_file_name("C:\\\\Users\\\\file.txt", PathType::Windows), "file.txt")
}

// Test 2: Environment Variable Handling
test "environment variable handling across platforms" {
  type Platform {
    Windows
    Linux
    MacOS
  }
  
  type EnvironmentVariable {
    name : String
    value : String
    platform_specific : Bool
  }
  
  let get_platform_specific_vars = fn(platform : Platform) : Array[EnvironmentVariable> {
    match platform {
      Platform::Windows => [
        { name: "PATH", value: "C:\\\\Windows\\\\system32;C:\\\\Program Files", platform_specific: true },
        { name: "USERPROFILE", value: "C:\\\\Users\\\\User", platform_specific: true },
        { name: "TEMP", value: "C:\\\\Users\\\\User\\\\AppData\\\\Local\\\\Temp", platform_specific: true },
        { name: "PROGRAMFILES", value: "C:\\\\Program Files", platform_specific: true }
      ]
      Platform::Linux => [
        { name: "PATH", value: "/usr/bin:/bin:/usr/local/bin", platform_specific: true },
        { name: "HOME", value: "/home/user", platform_specific: true },
        { name: "TEMP", value: "/tmp", platform_specific: true },
        { name: "LANG", value: "en_US.UTF-8", platform_specific: true }
      ]
      Platform::MacOS => [
        { name: "PATH", value: "/usr/bin:/bin:/usr/local/bin", platform_specific: true },
        { name: "HOME", value: "/Users/user", platform_specific: true },
        { name: "TMPDIR", value: "/tmp", platform_specific: true },
        { name: "LANG", value: "en_US.UTF-8", platform_specific: true }
      ]
    }
  }
  
  let get_common_vars = fn() : Array[EnvironmentVariable> {
    [
      { name: "APP_NAME", value: "Azimuth", platform_specific: false },
      { name: "APP_VERSION", value: "1.0.0", platform_specific: false },
      { name: "LOG_LEVEL", value: "INFO", platform_specific: false },
      { name: "PORT", value: "8080", platform_specific: false }
    ]
  }
  
  let get_all_env_vars = fn(platform : Platform) : Array[EnvironmentVariable> {
    let platform_vars = get_platform_specific_vars(platform)
    let common_vars = get_common_vars()
    let mut all_vars = []
    
    for var in platform_vars {
      all_vars = all_vars.push(var)
    }
    
    for var in common_vars {
      all_vars = all_vars.push(var)
    }
    
    all_vars
  }
  
  let get_env_var = fn(vars : Array[EnvironmentVariable], name : String) : Option[String] {
    for var in vars {
      if var.name == name {
        return Some(var.value)
      }
    }
    None
  }
  
  let set_env_var = fn(vars : Array[EnvironmentVariable], name : String, value : String) : Array[EnvironmentVariable] {
    let mut found = false
    let mut new_vars = []
    
    for var in vars {
      if var.name == name {
        new_vars = new_vars.push({ name: name, value: value, platform_specific: var.platform_specific })
        found = true
      } else {
        new_vars = new_vars.push(var)
      }
    }
    
    if !found {
      new_vars = new_vars.push({ name: name, value: value, platform_specific: false })
    }
    
    new_vars
  }
  
  let expand_path = fn(path : String, vars : Array[EnvironmentVariable]) : String {
    let mut result = path
    
    // Simple variable expansion for testing
    for var in vars {
      let pattern = "%" + var.name + "%"
      let unix_pattern = "$" + var.name
      
      if result.contains(pattern) {
        result = result.replace(pattern, var.value)
      }
      
      if result.contains(unix_pattern) {
        result = result.replace(unix_pattern, var.value)
      }
    }
    
    result
  }
  
  // Test platform-specific variables
  let windows_vars = get_platform_specific_vars(Platform::Windows)
  let linux_vars = get_platform_specific_vars(Platform::Linux)
  let macos_vars = get_platform_specific_vars(Platform::MacOS)
  
  // Test Windows-specific variables
  match get_env_var(windows_vars, "USERPROFILE") {
    Some(value) => assert_eq(value, "C:\\\\Users\\\\User")
    None => assert_true(false)
  }
  
  match get_env_var(windows_vars, "HOME") {
    Some(_) => assert_true(false)  // Should not exist on Windows
    None => assert_true(true)
  }
  
  // Test Linux-specific variables
  match get_env_var(linux_vars, "HOME") {
    Some(value) => assert_eq(value, "/home/user")
    None => assert_true(false)
  }
  
  match get_env_var(linux_vars, "USERPROFILE") {
    Some(_) => assert_true(false)  // Should not exist on Linux
    None => assert_true(true)
  }
  
  // Test macOS-specific variables
  match get_env_var(macos_vars, "TMPDIR") {
    Some(value) => assert_eq(value, "/tmp")
    None => assert_true(false)
  }
  
  match get_env_var(macos_vars, "TEMP") {
    Some(_) => assert_true(false)  // Should not exist on macOS
    None => assert_true(true)
  }
  
  // Test common variables
  let all_windows_vars = get_all_env_vars(Platform::Windows)
  let all_linux_vars = get_all_env_vars(Platform::Linux)
  
  match get_env_var(all_windows_vars, "APP_NAME") {
    Some(value) => assert_eq(value, "Azimuth")
    None => assert_true(false)
  }
  
  match get_env_var(all_linux_vars, "APP_NAME") {
    Some(value) => assert_eq(value, "Azimuth")
    None => assert_true(false)
  }
  
  // Test setting environment variables
  let updated_vars = set_env_var(all_windows_vars, "NEW_VAR", "new_value")
  
  match get_env_var(updated_vars, "NEW_VAR") {
    Some(value) => assert_eq(value, "new_value")
    None => assert_true(false)
  }
  
  // Test updating existing variable
  let updated_vars2 = set_env_var(updated_vars, "APP_NAME", "UpdatedAzimuth")
  
  match get_env_var(updated_vars2, "APP_NAME") {
    Some(value) => assert_eq(value, "UpdatedAzimuth")
    None => assert_true(false)
  }
  
  // Test path expansion
  let windows_path = "%USERPROFILE%\\\\Documents\\\\%APP_NAME%"
  let expanded_windows_path = expand_path(windows_path, all_windows_vars)
  assert_eq(expanded_windows_path, "C:\\\\Users\\\\User\\\\Documents\\\\Azimuth")
  
  let linux_path = "$HOME/Documents/$APP_NAME"
  let expanded_linux_path = expand_path(linux_path, all_linux_vars)
  assert_eq(expanded_linux_path, "/home/user/Documents/Azimuth")
}

// Test 3: Platform-Specific File Operations
test "platform-specific file operations" {
  type Platform {
    Windows
    Unix
  }
  
  type FileInfo {
    name : String
    size : Int
    is_directory : Bool
    is_hidden : Bool
    created_time : Int
    modified_time : Int
  }
  
  type FileSystem {
    platform : Platform
    mut files : Array[FileInfo>
  }
  
  let create_file_system = fn(platform : Platform) : FileSystem {
    let files = match platform {
      Platform::Windows => [
        { name: "file.txt", size: 1024, is_directory: false, is_hidden: false, created_time: 1000, modified_time: 2000 },
        { name: "hidden.txt", size: 512, is_directory: false, is_hidden: true, created_time: 1001, modified_time: 2001 },
        { name: "folder", size: 0, is_directory: true, is_hidden: false, created_time: 1002, modified_time: 2002 },
        { name: "system.dll", size: 2048, is_directory: false, is_hidden: true, created_time: 1003, modified_time: 2003 }
      ]
      Platform::Unix => [
        { name: "file.txt", size: 1024, is_directory: false, is_hidden: false, created_time: 1000, modified_time: 2000 },
        { name: ".hidden", size: 512, is_directory: false, is_hidden: true, created_time: 1001, modified_time: 2001 },
        { name: "folder", size: 0, is_directory: true, is_hidden: false, created_time: 1002, modified_time: 2002 },
        { name: "executable", size: 2048, is_directory: false, is_hidden: false, created_time: 1003, modified_time: 2003 }
      ]
    }
    
    { platform: platform, files: files }
  }
  
  let list_files = fn(fs : FileSystem, show_hidden : Bool) : Array<FileInfo> {
    let mut result = []
    
    for file in fs.files {
      if !file.is_hidden || show_hidden {
        result = result.push(file)
      }
    }
    
    result
  }
  
  let find_file = fn(fs : FileSystem, name : String) : Option<FileInfo> {
    for file in fs.files {
      if file.name == name {
        return Some(file)
      }
    }
    None
  }
  
  let get_file_extension = fn(fs : FileSystem, file : FileInfo) : String {
    if file.is_directory {
      return ""
    }
    
    let last_dot_pos = file.name.length() - 1
    let mut dot_pos = -1
    
    for i in 0..<file.name.length() {
      if file.name.substring(i, 1) == "." {
        dot_pos = i
      }
    }
    
    if dot_pos >= 0 && dot_pos < last_dot_pos {
      file.name.substring(dot_pos + 1, file.name.length() - dot_pos - 1)
    } else {
      ""
    }
  }
  
  let is_executable = fn(fs : FileSystem, file : FileInfo) : Bool {
    match fs.platform {
      Platform::Windows => {
        let ext = get_file_extension(fs, file)
        ext == "exe" || ext == "dll" || ext == "bat"
      }
      Platform::Unix => {
        // In Unix, executable permission would be checked, but for testing we'll check name
        file.name == "executable"
      }
    }
  }
  
  // Test Windows file system
  let windows_fs = create_file_system(Platform::Windows)
  
  // Test listing files
  let visible_files = list_files(windows_fs, false)
  assert_eq(visible_files.length(), 2)  // file.txt and folder
  
  let all_files = list_files(windows_fs, true)
  assert_eq(all_files.length(), 4)  // All files including hidden
  
  // Test finding files
  match find_file(windows_fs, "file.txt") {
    Some(file) => {
      assert_eq(file.name, "file.txt")
      assert_eq(file.size, 1024)
      assert_false(file.is_directory)
      assert_false(file.is_hidden)
    }
    None => assert_true(false)
  }
  
  match find_file(windows_fs, "hidden.txt") {
    Some(file) => {
      assert_eq(file.name, "hidden.txt")
      assert_true(file.is_hidden)
    }
    None => assert_true(false)
  }
  
  // Test executable detection
  match find_file(windows_fs, "system.dll") {
    Some(file) => {
      assert_true(is_executable(windows_fs, file))
    }
    None => assert_true(false)
  }
  
  // Test Unix file system
  let unix_fs = create_file_system(Platform::Unix)
  
  // Test listing files
  let visible_unix_files = list_files(unix_fs, false)
  assert_eq(visible_unix_files.length(), 3)  // file.txt, folder, and executable
  
  let all_unix_files = list_files(unix_fs, true)
  assert_eq(all_unix_files.length(), 4)  // All files including hidden
  
  // Test finding files
  match find_file(unix_fs, ".hidden") {
    Some(file) => {
      assert_eq(file.name, ".hidden")
      assert_true(file.is_hidden)
    }
    None => assert_true(false)
  }
  
  // Test executable detection
  match find_file(unix_fs, "executable") {
    Some(file) => {
      assert_true(is_executable(unix_fs, file))
    }
    None => assert_true(false)
  }
  
  match find_file(unix_fs, "file.txt") {
    Some(file) => {
      assert_false(is_executable(unix_fs, file))
    }
    None => assert_true(false)
  }
}

// Test 4: Platform-Specific Network Configuration
test "platform-specific network configuration" {
  type Platform {
    Windows
    Unix
  }
  
  type NetworkConfig {
    hostname : String
    dns_servers : Array[String]
    proxy_settings : Option[ProxyConfig>
    firewall_enabled : Bool
  }
  
  type ProxyConfig {
    http_proxy : String
    https_proxy : String
    no_proxy : Array[String]
  }
  
  let get_default_network_config = fn(platform : Platform) : NetworkConfig {
    match platform {
      Platform::Windows => {
        {
          hostname: "WINDOWS-PC",
          dns_servers: ["8.8.8.8", "8.8.4.4"],
          proxy_settings: Some({
            http_proxy: "http://proxy.company.com:8080",
            https_proxy: "https://proxy.company.com:8080",
            no_proxy: ["localhost", "127.0.0.1"]
          }),
          firewall_enabled: true
        }
      }
      Platform::Unix => {
        {
          hostname: "unix-host",
          dns_servers: ["1.1.1.1", "1.0.0.1"],
          proxy_settings: None,
          firewall_enabled: false
        }
      }
    }
  }
  
  let get_dns_config_file = fn(platform : Platform) : String {
    match platform {
      Platform::Windows => "C:\\\\Windows\\\\System32\\\\drivers\\\\etc\\\\hosts"
      Platform::Unix => "/etc/hosts"
    }
  }
  
  let get_network_interfaces_command = fn(platform : Platform) : String {
    match platform {
      Platform::Windows => "ipconfig /all"
      Platform::Unix => "ifconfig -a"
    }
  }
  
  let format_ip_address = fn(ip : String, platform : Platform) : String {
    match platform {
      Platform::Windows => {
        // Windows typically uses IPv4 addresses
        if ip.contains(":") {
          ""  // Ignore IPv6 for Windows in this test
        } else {
          ip
        }
      }
      Platform::Unix => {
        // Unix supports both IPv4 and IPv6
        ip
      }
    }
  }
  
  let ping_command = fn(host : String, platform : Platform) : String {
    match platform {
      Platform::Windows => "ping -n 4 " + host
      Platform::Unix => "ping -c 4 " + host
    }
  }
  
  // Test Windows network configuration
  let windows_config = get_default_network_config(Platform::Windows)
  
  assert_eq(windows_config.hostname, "WINDOWS-PC")
  assert_eq(windows_config.dns_servers, ["8.8.8.8", "8.8.4.4"])
  assert_true(windows_config.firewall_enabled)
  
  match windows_config.proxy_settings {
    Some(proxy) => {
      assert_eq(proxy.http_proxy, "http://proxy.company.com:8080")
      assert_eq(proxy.https_proxy, "https://proxy.company.com:8080")
      assert_eq(proxy.no_proxy, ["localhost", "127.0.0.1"])
    }
    None => assert_true(false)
  }
  
  // Test Unix network configuration
  let unix_config = get_default_network_config(Platform::Unix)
  
  assert_eq(unix_config.hostname, "unix-host")
  assert_eq(unix_config.dns_servers, ["1.1.1.1", "1.0.0.1"])
  assert_false(unix_config.firewall_enabled)
  
  match unix_config.proxy_settings {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test platform-specific paths and commands
  assert_eq(get_dns_config_file(Platform::Windows), "C:\\\\Windows\\\\System32\\\\drivers\\\\etc\\\\hosts")
  assert_eq(get_dns_config_file(Platform::Unix), "/etc/hosts")
  
  assert_eq(get_network_interfaces_command(Platform::Windows), "ipconfig /all")
  assert_eq(get_network_interfaces_command(Platform::Unix), "ifconfig -a")
  
  // Test IP address formatting
  assert_eq(format_ip_address("192.168.1.1", Platform::Windows), "192.168.1.1")
  assert_eq(format_ip_address("2001:db8::1", Platform::Windows), "")
  assert_eq(format_ip_address("192.168.1.1", Platform::Unix), "192.168.1.1")
  assert_eq(format_ip_address("2001:db8::1", Platform::Unix), "2001:db8::1")
  
  // Test ping command generation
  assert_eq(ping_command("example.com", Platform::Windows), "ping -n 4 example.com")
  assert_eq(ping_command("example.com", Platform::Unix), "ping -c 4 example.com")
}