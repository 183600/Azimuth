// Cross-Platform Compatibility Tests for Azimuth Telemetry System
// This file contains test cases for cross-platform compatibility and adaptation

// Test 1: Operating System Detection
test "operating system detection and adaptation" {
  // Simulate OS detection
  let detect_os = fn() {
    // In real implementation, this would check system properties
    let os_info = {
      "platform": "linux",
      "version": "5.15.0",
      "architecture": "x86_64"
    }
    os_info
  }
  
  let os_info = detect_os()
  assert_eq(os_info["platform"], "linux")
  assert_eq(os_info["version"], "5.15.0")
  assert_eq(os_info["architecture"], "x86_64")
  
  // Test OS-specific path handling
  let get_path_separator = fn(platform) {
    match platform {
      "windows" => "\\",
      _ => "/"
    }
  }
  
  let separator = get_path_separator(os_info["platform"])
  assert_eq(separator, "/")
  
  // Test path joining
  let join_paths = fn(parts, separator) {
    parts.reduce(fn(acc, part) { acc + separator + part }, "")
  }
  
  let path_parts = ["home", "user", "azimuth", "data"]
  let joined_path = join_paths(path_parts, separator)
  assert_eq(joined_path, "/home/user/azimuth/data")
  
  // Test Windows path joining
  let windows_separator = get_path_separator("windows")
  let windows_path = join_paths(path_parts, windows_separator)
  assert_eq(windows_path, "\\home\\user\\azimuth\\data")
}

// Test 2: File System Compatibility
test "file system compatibility across platforms" {
  // Simulate file system operations
  let file_system = {}
  
  // Platform-specific file permissions
  let get_default_permissions = fn(platform) {
    match platform {
      "windows" => "644",
      "linux" => "755",
      "macos" => "755",
      _ => "644"
    }
  }
  
  let create_file = fn(path, content, platform) {
    let permissions = get_default_permissions(platform)
    file_system = file_system.set(path, {
      "content": content,
      "permissions": permissions,
      "platform": platform
    })
    Ok("File created")
  }
  
  // Test file creation on different platforms
  let platforms = ["linux", "windows", "macos"]
  let creation_results = []
  
  for platform in platforms {
    let file_path = "/tmp/azimuth_" + platform + ".log"
    let result = create_file(file_path, "Telemetry data for " + platform, platform)
    creation_results = creation_results.push(result)
  }
  
  // Verify file creation
  for result in creation_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // Test file reading with platform-specific handling
  let read_file = fn(path) {
    match file_system.get(path) {
      Some(file_info) => Ok(file_info["content"])
      None => Err("File not found")
    }
  }
  
  let linux_file = read_file("/tmp/azimuth_linux.log")
  match linux_file {
    Ok(content) => assert_eq(content, "Telemetry data for linux")
    Err(_) => assert_true(false)
  }
  
  // Test file permissions
  let get_file_permissions = fn(path) {
    match file_system.get(path) {
      Some(file_info) => Ok(file_info["permissions"])
      None => Err("File not found")
    }
  }
  
  let linux_permissions = get_file_permissions("/tmp/azimuth_linux.log")
  match linux_permissions {
    Ok(permissions) => assert_eq(permissions, "755")
    Err(_) => assert_true(false)
  }
  
  let windows_permissions = get_file_permissions("/tmp/azimuth_windows.log")
  match windows_permissions {
    Ok(permissions) => assert_eq(permissions, "644")
    Err(_) => assert_true(false)
  }
}

// Test 3: Network Configuration Compatibility
test "network configuration compatibility" {
  // Simulate network configuration for different platforms
  let get_network_config = fn(platform) {
    match platform {
      "linux" => {
        "interface": "eth0",
        "dns_servers": ["8.8.8.8", "8.8.4.4"],
        "timeout": 30,
        "retry_count": 3
      }
      "windows" => {
        "interface": "Ethernet",
        "dns_servers": ["8.8.8.8", "8.8.4.4"],
        "timeout": 60,
        "retry_count": 5
      }
      "macos" => {
        "interface": "en0",
        "dns_servers": ["8.8.8.8", "8.8.4.4"],
        "timeout": 45,
        "retry_count": 4
      }
      _ => {
        "interface": "default",
        "dns_servers": ["8.8.8.8"],
        "timeout": 30,
        "retry_count": 3
      }
    }
  }
  
  // Test network configuration retrieval
  let linux_config = get_network_config("linux")
  assert_eq(linux_config["interface"], "eth0")
  assert_eq(linux_config["timeout"], 30)
  
  let windows_config = get_network_config("windows")
  assert_eq(windows_config["interface"], "Ethernet")
  assert_eq(windows_config["timeout"], 60)
  
  let macos_config = get_network_config("macos")
  assert_eq(macos_config["interface"], "en0")
  assert_eq(macos_config["timeout"], 45)
  
  // Test platform-specific network operations
  let configure_network = fn(platform, config) {
    match platform {
      "linux" => "ifconfig " + config["interface"] + " up"
      "windows" => "netsh interface set interface \"" + config["interface"] + "\" enabled"
      "macos" => "ifconfig " + config["interface"] + " up"
      _ => "echo Network configuration not supported"
    }
  }
  
  let linux_command = configure_network("linux", linux_config)
  assert_eq(linux_command, "ifconfig eth0 up")
  
  let windows_command = configure_network("windows", windows_config)
  assert_eq(windows_command, "netsh interface set interface \"Ethernet\" enabled")
}

// Test 4: Environment Variable Handling
test "environment variable handling across platforms" {
  // Simulate environment variables
  let environment_vars = {}
  
  // Set platform-specific environment variables
  let set_env_var = fn(key, value, platform) {
    let platform_key = platform + ":" + key
    environment_vars = environment_vars.set(platform_key, value)
    Ok("Environment variable set")
  }
  
  let get_env_var = fn(key, platform) {
    let platform_key = platform + ":" + key
    match environment_vars.get(platform_key) {
      Some(value) => Ok(value)
      None => Err("Environment variable not found")
    }
  }
  
  // Test environment variable operations
  let env_vars = [
    ("PATH", "/usr/bin:/bin", "linux"),
    ("PATH", "C:\\Windows\\System32", "windows"),
    ("PATH", "/usr/bin:/bin:/usr/local/bin", "macos"),
    ("HOME", "/home/user", "linux"),
    ("USERPROFILE", "C:\\Users\\User", "windows"),
    ("HOME", "/Users/user", "macos")
  ]
  
  let set_results = []
  for env_var in env_vars {
    let result = set_env_var(env_var.0, env_var.1, env_var.2)
    set_results = set_results.push(result)
  }
  
  // Verify environment variable setting
  for result in set_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // Test environment variable retrieval
  let linux_path = get_env_var("PATH", "linux")
  match linux_path {
    Ok(path) => assert_eq(path, "/usr/bin:/bin")
    Err(_) => assert_true(false)
  }
  
  let windows_path = get_env_var("PATH", "windows")
  match windows_path {
    Ok(path) => assert_eq(path, "C:\\Windows\\System32")
    Err(_) => assert_true(false)
  }
  
  let macos_home = get_env_var("HOME", "macos")
  match macos_home {
    Ok(home) => assert_eq(home, "/Users/user")
    Err(_) => assert_true(false)
  }
  
  // Test cross-platform environment variable access
  let get_cross_platform_home = fn(platform) {
    match platform {
      "linux" | "macos" => get_env_var("HOME", platform),
      "windows" => get_env_var("USERPROFILE", platform),
      _ => Err("Unsupported platform")
    }
  }
  
  let linux_home = get_cross_platform_home("linux")
  match linux_home {
    Ok(home) => assert_eq(home, "/home/user")
    Err(_) => assert_true(false)
  }
  
  let windows_home = get_cross_platform_home("windows")
  match windows_home {
    Ok(home) => assert_eq(home, "C:\\Users\\User")
    Err(_) => assert_true(false)
  }
}

// Test 5: Process Management Compatibility
test "process management compatibility" {
  // Simulate process management operations
  let running_processes = []
  
  let start_process = fn(command, args, platform) {
    let process_id = running_processes.length() + 1
    let process_info = {
      "id": process_id,
      "command": command,
      "args": args,
      "platform": platform,
      "status": "running"
    }
    running_processes = running_processes.push(process_info)
    Ok(process_id)
  }
  
  let stop_process = fn(process_id) {
    let process_index = running_processes.index_of(fn(p) { p["id"] == process_id }).or_else(-1)
    if process_index >= 0 {
      running_processes = running_processes.with(process_index, 
        running_processes[process_index].set("status", "stopped"))
      Ok("Process stopped")
    } else {
      Err("Process not found")
    }
  }
  
  // Test platform-specific process commands
  let get_platform_command = fn(operation, platform) {
    match (operation, platform) {
      ("list", "linux") => "ps aux",
      ("list", "windows") => "tasklist",
      ("list", "macos") => "ps aux",
      ("kill", "linux") => "kill -9",
      ("kill", "windows") => "taskkill /F /PID",
      ("kill", "macos") => "kill -9",
      _ => "echo Unsupported operation"
    }
  }
  
  // Test process commands
  let linux_list = get_platform_command("list", "linux")
  assert_eq(linux_list, "ps aux")
  
  let windows_list = get_platform_command("list", "windows")
  assert_eq(windows_list, "tasklist")
  
  let macos_kill = get_platform_command("kill", "macos")
  assert_eq(macos_kill, "kill -9")
  
  // Test process operations
  let result1 = start_process("azimuth-agent", ["--config", "config.yaml"], "linux")
  let result2 = start_process("azimuth-agent.exe", ["--config", "config.yaml"], "windows")
  let result3 = start_process("azimuth-agent", ["--config", "config.yaml"], "macos")
  
  match (result1, result2, result3) {
    (Ok(id1), Ok(id2), Ok(id3)) => {
      assert_eq(id1, 1)
      assert_eq(id2, 2)
      assert_eq(id3, 3)
    }
    _ => assert_true(false)
  }
  
  assert_eq(running_processes.length(), 3)
  
  // Test process stopping
  let stop_result = stop_process(2)
  match stop_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Verify process status
  let windows_process = running_processes[1]
  assert_eq(windows_process["status"], "stopped")
}

// Test 6: Service Integration Compatibility
test "service integration compatibility" {
  // Simulate service integration
  let service_configs = {}
  
  let configure_service = fn(service_name, config, platform) {
    let platform_config = match platform {
      "linux" => {
        "service_file": "/etc/systemd/system/" + service_name + ".service",
        "start_command": "systemctl start " + service_name,
        "stop_command": "systemctl stop " + service_name,
        "status_command": "systemctl status " + service_name
      }
      "windows" => {
        "service_file": "C:\\Services\\" + service_name + ".exe",
        "start_command": "net start " + service_name,
        "stop_command": "net stop " + service_name,
        "status_command": "sc query " + service_name
      }
      "macos" => {
        "service_file": "/Library/LaunchDaemons/" + service_name + ".plist",
        "start_command": "launchctl load " + service_name,
        "stop_command": "launchctl unload " + service_name,
        "status_command": "launchctl list " + service_name
      }
      _ => {
        "service_file": "/tmp/" + service_name + ".service",
        "start_command": "echo Starting " + service_name,
        "stop_command": "echo Stopping " + service_name,
        "status_command": "echo Status of " + service_name
      }
    }
    
    service_configs = service_configs.set(service_name + ":" + platform, platform_config)
    Ok("Service configured")
  }
  
  // Test service configuration
  let services = ["azimuth-collector", "azimuth-processor", "azimuth-exporter"]
  let platforms = ["linux", "windows", "macos"]
  
  let config_results = []
  for service in services {
    for platform in platforms {
      let result = configure_service(service, {}, platform)
      config_results = config_results.push(result)
    }
  }
  
  // Verify service configuration
  for result in config_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // Test service command retrieval
  let get_service_command = fn(service_name, operation, platform) {
    let key = service_name + ":" + platform
    match service_configs.get(key) {
      Some(config) => {
        match operation {
          "start" => Ok(config["start_command"])
          "stop" => Ok(config["stop_command"])
          "status" => Ok(config["status_command"])
          _ => Err("Unknown operation")
        }
      }
      None => Err("Service not configured")
    }
  }
  
  // Test service commands
  let linux_start = get_service_command("azimuth-collector", "start", "linux")
  match linux_start {
    Ok(command) => assert_eq(command, "systemctl start azimuth-collector")
    Err(_) => assert_true(false)
  }
  
  let windows_stop = get_service_command("azimuth-processor", "stop", "windows")
  match windows_stop {
    Ok(command) => assert_eq(command, "net stop azimuth-processor")
    Err(_) => assert_true(false)
  }
  
  let macos_status = get_service_command("azimuth-exporter", "status", "macos")
  match macos_status {
    Ok(command) => assert_eq(command, "launchctl list azimuth-exporter")
    Err(_) => assert_true(false)
  }
}

// Test 7: Logging System Compatibility
test "logging system compatibility" {
  // Simulate logging system
  let log_entries = []
  
  let get_log_path = fn(platform, log_name) {
    match platform {
      "linux" => "/var/log/azimuth/" + log_name + ".log",
      "windows" => "C:\\ProgramData\\Azimuth\\Logs\\" + log_name + ".log",
      "macos" => "/var/log/azimuth/" + log_name + ".log",
      _ => "/tmp/azimuth_" + log_name + ".log"
    }
  }
  
  let write_log = fn(level, message, platform, log_name) {
    let log_path = get_log_path(platform, log_name)
    let timestamp = 1234567890
    let log_entry = {
      "timestamp": timestamp,
      "level": level,
      "message": message,
      "path": log_path,
      "platform": platform
    }
    log_entries = log_entries.push(log_entry)
    Ok("Log written")
  }
  
  // Test log path generation
  let linux_log_path = get_log_path("linux", "collector")
  assert_eq(linux_log_path, "/var/log/azimuth/collector.log")
  
  let windows_log_path = get_log_path("windows", "processor")
  assert_eq(windows_log_path, "C:\\ProgramData\\Azimuth\\Logs\\processor.log")
  
  let macos_log_path = get_log_path("macos", "exporter")
  assert_eq(macos_log_path, "/var/log/azimuth/exporter.log")
  
  // Test log writing
  let log_operations = [
    ("INFO", "Service started", "linux", "collector"),
    ("WARN", "High memory usage", "windows", "processor"),
    ("ERROR", "Connection failed", "macos", "exporter"),
    ("DEBUG", "Processing data", "linux", "collector"),
    ("INFO", "Service stopped", "windows", "processor")
  ]
  
  let log_results = []
  for operation in log_operations {
    let result = write_log(operation.0, operation.1, operation.2, operation.3)
    log_results = log_results.push(result)
  }
  
  // Verify log writing
  for result in log_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(log_entries.length(), 5)
  
  // Test log filtering by platform
  let linux_logs = log_entries.filter(fn(entry) { entry["platform"] == "linux" })
  assert_eq(linux_logs.length(), 2)
  
  let windows_logs = log_entries.filter(fn(entry) { entry["platform"] == "windows" })
  assert_eq(windows_logs.length(), 2)
  
  let macos_logs = log_entries.filter(fn(entry) { entry["platform"] == "macos" })
  assert_eq(macos_logs.length(), 1)
}

// Test 8: Configuration File Compatibility
test "configuration file compatibility" {
  // Simulate configuration file handling
  let config_files = {}
  
  let get_config_path = fn(platform, config_name) {
    match platform {
      "linux" => "/etc/azimuth/" + config_name + ".yaml",
      "windows" => "C:\\Program Files\\Azimuth\\Config\\" + config_name + ".yaml",
      "macos" => "/etc/azimuth/" + config_name + ".yaml",
      _ => "/tmp/azimuth_" + config_name + ".yaml"
    }
  }
  
  let load_config = fn(platform, config_name) {
    let config_path = get_config_path(platform, config_name)
    let default_config = {
      "telemetry": {
        "enabled": true,
        "endpoint": "http://localhost:8080",
        "batch_size": 100
      },
      "logging": {
        "level": "INFO",
        "file": "azimuth.log"
      }
    }
    
    config_files = config_files.set(config_path, default_config)
    Ok(default_config)
  }
  
  // Test config path generation
  let linux_config = get_config_path("linux", "azimuth")
  assert_eq(linux_config, "/etc/azimuth/azimuth.yaml")
  
  let windows_config = get_config_path("windows", "azimuth")
  assert_eq(windows_config, "C:\\Program Files\\Azimuth\\Config\\azimuth.yaml")
  
  // Test config loading
  let platforms = ["linux", "windows", "macos"]
  let config_results = []
  
  for platform in platforms {
    let result = load_config(platform, "azimuth")
    config_results = config_results.push(result)
  }
  
  // Verify config loading
  for result in config_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // Test config access
  let linux_config_data = load_config("linux", "azimuth")
  match linux_config_data {
    Ok(config) => {
      assert_eq(config["telemetry"]["enabled"], true)
      assert_eq(config["telemetry"]["endpoint"], "http://localhost:8080")
      assert_eq(config["logging"]["level"], "INFO")
    }
    Err(_) => assert_true(false)
  }
  
  // Test platform-specific config overrides
  let get_platform_specific_config = fn(platform, base_config) {
    match platform {
      "windows" => {
        base_config.set("telemetry", base_config["telemetry"].set("endpoint", "http://localhost:9090"))
      }
      "linux" => {
        base_config.set("logging", base_config["logging"].set("file", "/var/log/azimuth/azimuth.log"))
      }
      "macos" => {
        base_config.set("telemetry", base_config["telemetry"].set("batch_size", 200))
      }
      _ => base_config
    }
  }
  
  let base_config = {
    "telemetry": {
      "enabled": true,
      "endpoint": "http://localhost:8080",
      "batch_size": 100
    },
    "logging": {
      "level": "INFO",
      "file": "azimuth.log"
    }
  }
  
  let windows_specific = get_platform_specific_config("windows", base_config)
  assert_eq(windows_specific["telemetry"]["endpoint"], "http://localhost:9090")
  
  let linux_specific = get_platform_specific_config("linux", base_config)
  assert_eq(linux_specific["logging"]["file"], "/var/log/azimuth/azimuth.log")
  
  let macos_specific = get_platform_specific_config("macos", base_config)
  assert_eq(macos_specific["telemetry"]["batch_size"], 200)
}

// Test 9: Performance Metrics Compatibility
test "performance metrics compatibility" {
  // Simulate performance metrics collection
  let performance_metrics = {}
  
  let get_cpu_usage_command = fn(platform) {
    match platform {
      "linux" => "top -bn1 | grep 'Cpu(s)' | awk '{print $2}' | cut -d'%' -f1",
      "windows" => "wmic cpu get loadpercentage /value",
      "macos" => "top -l 1 | grep 'CPU usage' | awk '{print $3}' | cut -d'%' -f1",
      _ => "echo 0"
    }
  }
  
  let get_memory_usage_command = fn(platform) {
    match platform {
      "linux" => "free -m | grep 'Mem:' | awk '{print $3/$2 * 100.0}'",
      "windows" => "wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /value",
      "macos" => "vm_stat | grep 'Pages free' | awk '{print $3}' | sed 's/\.//'",
      _ => "echo 0"
    }
  }
  
  let collect_metrics = fn(platform) {
    let cpu_command = get_cpu_usage_command(platform)
    let memory_command = get_memory_usage_command(platform)
    
    let metrics = {
      "platform": platform,
      "cpu_command": cpu_command,
      "memory_command": memory_command,
      "timestamp": 1234567890
    }
    
    performance_metrics = performance_metrics.set(platform + ":" + 1234567890, metrics)
    Ok("Metrics collected")
  }
  
  // Test metric command generation
  let linux_cpu = get_cpu_usage_command("linux")
  assert_eq(linux_cpu, "top -bn1 | grep 'Cpu(s)' | awk '{print $2}' | cut -d'%' -f1")
  
  let windows_memory = get_memory_usage_command("windows")
  assert_eq(windows_memory, "wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /value")
  
  let macos_cpu = get_cpu_usage_command("macos")
  assert_eq(macos_cpu, "top -l 1 | grep 'CPU usage' | awk '{print $3}' | cut -d'%' -f1")
  
  // Test metrics collection
  let platforms = ["linux", "windows", "macos"]
  let collection_results = []
  
  for platform in platforms {
    let result = collect_metrics(platform)
    collection_results = collection_results.push(result)
  }
  
  // Verify metrics collection
  for result in collection_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(performance_metrics.length(), 3)
  
  // Test metrics retrieval
  let get_metrics = fn(platform) {
    let key = platform + ":" + 1234567890
    match performance_metrics.get(key) {
      Some(metrics) => Ok(metrics)
      None => Err("Metrics not found")
    }
  }
  
  let linux_metrics = get_metrics("linux")
  match linux_metrics {
    Ok(metrics) => {
      assert_eq(metrics["platform"], "linux")
      assert_eq(metrics["cpu_command"], "top -bn1 | grep 'Cpu(s)' | awk '{print $2}' | cut -d'%' -f1")
    }
    Err(_) => assert_true(false)
  }
}

// Test 10: Package Management Compatibility
test "package management compatibility" {
  // Simulate package management operations
  let package_managers = {
    "linux": {
      "install": "apt-get install",
      "remove": "apt-get remove",
      "update": "apt-get update",
      "search": "apt-cache search"
    },
    "windows": {
      "install": "choco install",
      "remove": "choco uninstall",
      "update": "choco upgrade",
      "search": "choco search"
    },
    "macos": {
      "install": "brew install",
      "remove": "brew uninstall",
      "update": "brew update",
      "search": "brew search"
    }
  }
  
  let get_package_command = fn(operation, platform) {
    match package_managers.get(platform) {
      Some(manager) => {
        match manager.get(operation) {
          Some(command) => Ok(command)
          None => Err("Operation not supported")
        }
      }
      None => Err("Platform not supported")
    }
  }
  
  // Test package command retrieval
  let linux_install = get_package_command("install", "linux")
  match linux_install {
    Ok(command) => assert_eq(command, "apt-get install")
    Err(_) => assert_true(false)
  }
  
  let windows_search = get_package_command("search", "windows")
  match windows_search {
    Ok(command) => assert_eq(command, "choco search")
    Err(_) => assert_true(false)
  }
  
  let macos_remove = get_package_command("remove", "macos")
  match macos_remove {
    Ok(command) => assert_eq(command, "brew uninstall")
    Err(_) => assert_true(false)
  }
  
  // Test package installation simulation
  let installed_packages = {}
  
  let install_package = fn(package_name, platform) {
    let command_result = get_package_command("install", platform)
    match command_result {
      Ok(command) => {
        let package_key = platform + ":" + package_name
        installed_packages = installed_packages.set(package_key, {
          "name": package_name,
          "platform": platform,
          "install_command": command + " " + package_name,
          "installed_at": 1234567890
        })
        Ok("Package installed")
      }
      Err(error) => Err(error)
    }
  }
  
  // Test package installations
  let packages = [
    ("curl", "linux"),
    ("git", "windows"),
    ("docker", "macos"),
    ("python3", "linux"),
    ("nodejs", "windows")
  ]
  
  let install_results = []
  for package in packages {
    let result = install_package(package.0, package.1)
    install_results = install_results.push(result)
  }
  
  // Verify package installations
  for result in install_results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  assert_eq(installed_packages.length(), 5)
  
  // Test package listing by platform
  let get_installed_packages = fn(platform) {
    installed_packages.filter(fn(entry) { 
      entry.0.contains(platform + ":") 
    }).values()
  }
  
  let linux_packages = get_installed_packages("linux")
  assert_eq(linux_packages.length(), 2)
  
  let windows_packages = get_installed_packages("windows")
  assert_eq(windows_packages.length(), 2)
  
  let macos_packages = get_installed_packages("macos")
  assert_eq(macos_packages.length(), 1)
  
  // Verify specific packages
  let curl_package = installed_packages.get("linux:curl")
  match curl_package {
    Some(package_info) => {
      assert_eq(package_info["name"], "curl")
      assert_eq(package_info["platform"], "linux")
      assert_eq(package_info["install_command"], "apt-get install curl")
    }
    None => assert_true(false)
  }
}