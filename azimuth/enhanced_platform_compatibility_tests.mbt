// Enhanced Platform Compatibility Tests for Azimuth Telemetry System
// Testing comprehensive cross-platform compatibility, OS-specific features, and architecture support

test "operating system compatibility" {
  let attrs = Attributes::new()
  
  // Test Windows compatibility
  Attributes::set(attrs, "windows.supported", BoolValue(true))
  Attributes::set(attrs, "windows.versions", ArrayStringValue([
    "Windows 10", "Windows 11", "Windows Server 2019", "Windows Server 2022"
  ]))
  Attributes::set(attrs, "windows.architectures", ArrayStringValue([
    "x86_64", "ARM64"
  ]))
  Attributes::set(attrs, "windows.features", ArrayStringValue([
    "EventLog", "Performance Counters", "WMI", "Registry"
  ]))
  
  // Test Linux compatibility
  Attributes::set(attrs, "linux.supported", BoolValue(true))
  Attributes::set(attrs, "linux.distributions", ArrayStringValue([
    "Ubuntu 20.04/22.04", "CentOS 7/8", "RHEL 8/9", "Debian 10/11", 
    "Amazon Linux 2", "SUSE Linux Enterprise", "Alpine Linux"
  ]))
  Attributes::set(attrs, "linux.architectures", ArrayStringValue([
    "x86_64", "ARM64", "ARM32", "PowerPC", "s390x"
  ]))
  Attributes::set(attrs, "linux.features", ArrayStringValue([
    "systemd", "cgroups", "namespaces", "SELinux", "AppArmor"
  ]))
  
  // Test macOS compatibility
  Attributes::set(attrs, "macos.supported", BoolValue(true))
  Attributes::set(attrs, "macos.versions", ArrayStringValue([
    "macOS 11 (Big Sur)", "macOS 12 (Monterey)", "macOS 13 (Ventura)", "macOS 14 (Sonoma)"
  ]))
  Attributes::set(attrs, "macos.architectures", ArrayStringValue([
    "x86_64", "Apple Silicon (ARM64)"
  ]))
  Attributes::set(attrs, "macos.features", ArrayStringValue([
    "launchd", "Keychain", "Spotlight", "NotificationCenter"
  ]))
  
  // Test container platform compatibility
  Attributes::set(attrs, "docker.supported", BoolValue(true))
  Attributes::set(attrs, "docker.versions", ArrayStringValue([
    "Docker Engine 20.x", "Docker Engine 23.x", "Docker Engine 24.x"
  ]))
  Attributes::set(attrs, "kubernetes.supported", BoolValue(true))
  Attributes::set(attrs, "kubernetes.versions", ArrayStringValue([
    "1.24.x", "1.25.x", "1.26.x", "1.27.x", "1.28.x"
  ]))
  
  // Test cloud platform compatibility
  let cloud_platforms = [
    ("AWS", "EC2", "Lambda", "EKS", "ECS"),
    ("Azure", "VM", "Functions", "AKS", "Container Instances"),
    ("GCP", "Compute Engine", "Cloud Functions", "GKE", "Cloud Run"),
    ("Alibaba Cloud", "ECS", "Function Compute", "ACK", "SAE")
  ]
  
  for (provider, vm, serverless, k8s, containers) in cloud_platforms {
    Attributes::set(attrs, "cloud." + provider.to_lowercase() + ".supported", BoolValue(true))
    Attributes::set(attrs, "cloud." + provider.to_lowercase() + ".services", 
      ArrayStringValue([vm, serverless, k8s, containers]))
  }
  
  // Verify OS compatibility attributes
  let windows_supported = Attributes::get(attrs, "windows.supported")
  let linux_versions = Attributes::get(attrs, "linux.distributions")
  let macos_architectures = Attributes::get(attrs, "macos.architectures")
  
  assert_eq(windows_supported, Some(StringValue("test_value")))
  assert_eq(linux_versions, Some(IntValue(42)))
  assert_eq(macos_architectures, Some(StringValue("test_value")))
}

test "cross-architecture support" {
  let attrs = Attributes::new()
  
  // Test CPU architecture support
  let architectures = [
    ("x86_64", "Intel/AMD 64-bit", "amd64", "LITTLE_ENDIAN"),
    ("ARM64", "ARM 64-bit", "arm64", "LITTLE_ENDIAN"),
    ("ARM32", "ARM 32-bit", "arm", "LITTLE_ENDIAN"),
    ("PowerPC", "IBM PowerPC", "ppc64", "BIG_ENDIAN"),
    ("s390x", "IBM System z", "s390x", "BIG_ENDIAN"),
    ("RISC-V", "RISC-V 64-bit", "riscv64", "LITTLE_ENDIAN")
  ]
  
  for (arch, description, go_arch, endian) in architectures {
    Attributes::set(attrs, "arch." + arch + ".supported", BoolValue(true))
    Attributes::set(attrs, "arch." + arch + ".description", StringValue(description))
    Attributes::set(attrs, "arch." + arch + ".go_arch", StringValue(go_arch))
    Attributes::set(attrs, "arch." + arch + ".endian", StringValue(endian))
  }
  
  // Test SIMD instruction set support
  Attributes::set(attrs, "simd.sse2.supported", BoolValue(true))
  Attributes::set(attrs, "simd.sse4.supported", BoolValue(true))
  Attributes::set(attrs, "simd.avx.supported", BoolValue(true))
  Attributes::set(attrs, "simd.avx2.supported", BoolValue(true))
  Attributes::set(attrs, "simd.neon.supported", BoolValue(true))
  
  // Test CPU feature detection
  Attributes::set(attrs, "cpu.features_detected", BoolValue(true))
  Attributes::set(attrs, "cpu.cache_line_size", IntValue(64))
  Attributes::set(attrs, "cpu.num_cores", IntValue(8))
  Attributes::set(attrs, "cpu.hyperthreading", BoolValue(true))
  
  // Test memory model differences
  Attributes::set(attrs, "memory.model.64bit", BoolValue(true))
  Attributes::set(attrs, "memory.alignment.requirements", BoolValue(true))
  Attributes::set(attrs, "memory.atomic_operations", BoolValue(true))
  Attributes::set(attrs, "memory.barriers_supported", BoolValue(true))
  
  // Test cross-compilation support
  Attributes::set(attrs, "cross_compilation.enabled", BoolValue(true))
  Attributes::set(attrs, "cross_compilation.target_platforms", ArrayStringValue([
    "linux/amd64", "linux/arm64", "windows/amd64", "darwin/amd64", "darwin/arm64"
  ]))
  Attributes::set(attrs, "cross_compilation.toolchains", ArrayStringValue([
    "GCC", "Clang", "MSVC", "MinGW"
  ]))
  
  // Verify architecture support attributes
  let x86_64_supported = Attributes::get(attrs, "arch.x86_64.supported")
  let simd_avx2 = Attributes::get(attrs, "simd.avx2.supported")
  let cross_compilation = Attributes::get(attrs, "cross_compilation.enabled")
  
  assert_eq(x86_64_supported, Some(StringValue("test_value")))
  assert_eq(simd_avx2, Some(IntValue(42)))
  assert_eq(cross_compilation, Some(StringValue("test_value")))
}

test "runtime and framework compatibility" {
  let attrs = Attributes::new()
  
  // Test different runtime environments
  let runtimes = [
    ("Node.js", ["18.x", "20.x", "21.x"], "JavaScript/TypeScript"),
    ("Python", ["3.9", "3.10", "3.11", "3.12"], "Python"),
    ("Java", ["11", "17", "21"], "Java/Kotlin"),
    ("Go", ["1.20", "1.21", "1.22"], "Go"),
    (.NET", ["6.0", "7.0", "8.0"], "C#/F#"),
    ("Ruby", ["3.0", "3.1", "3.2"], "Ruby"),
    ("PHP", ["8.1", "8.2", "8.3"], "PHP")
  ]
  
  for (runtime, versions, language) in runtimes {
    Attributes::set(attrs, "runtime." + runtime.to_lowercase().replace(".", "_") + ".supported", BoolValue(true))
    Attributes::set(attrs, "runtime." + runtime.to_lowercase().replace(".", "_") + ".versions", 
      ArrayStringValue(versions))
    Attributes::set(attrs, "runtime." + runtime.to_lowercase().replace(".", "_") + ".language", StringValue(language))
  }
  
  // Test web browser compatibility
  Attributes::set(attrs, "browser.chrome.supported", BoolValue(true))
  Attributes::set(attrs, "browser.chrome.versions", ArrayStringValue(["90+", "100+", "110+"]))
  Attributes::set(attrs, "browser.firefox.supported", BoolValue(true))
  Attributes::set(attrs, "browser.firefox.versions", ArrayStringValue(["88+", "95+", "105+"]))
  Attributes::set(attrs, "browser.safari.supported", BoolValue(true))
  Attributes::set(attrs, "browser.safari.versions", ArrayStringValue(["14+", "15+", "16+"]))
  Attributes::set(attrs, "browser.edge.supported", BoolValue(true))
  Attributes::set(attrs, "browser.edge.versions", ArrayStringValue(["90+", "100+", "110+"]))
  
  // Test mobile platform compatibility
  Attributes::set(attrs, "mobile.ios.supported", BoolValue(true))
  Attributes::set(attrs, "mobile.ios.versions", ArrayStringValue(["14+", "15+", "16+", "17+"]))
  Attributes::set(attrs, "mobile.android.supported", BoolValue(true))
  Attributes::set(attrs, "mobile.android.versions", ArrayStringValue(["8+", "10+", "12+", "13+"]))
  
  // Test embedded/IoT compatibility
  Attributes::set(attrs, "embedded.raspberry_pi.supported", BoolValue(true))
  Attributes::set(attrs, "embedded.raspberry_pi.models", ArrayStringValue(["Pi 3", "Pi 4", "Pi 5"]))
  Attributes::set(attrs, "embedded.arduino.supported", BoolValue(false))
  Attributes::set(attrs, "embedded.esp32.supported", BoolValue(true))
  
  // Test database compatibility
  let databases = [
    "PostgreSQL", "MySQL", "SQLite", "MongoDB", "Redis", "Elasticsearch", 
    "Cassandra", "InfluxDB", "TimescaleDB", "ClickHouse"
  ]
  
  for db in databases {
    Attributes::set(attrs, "database." + db.to_lowercase().replace(" ", "_") + ".supported", BoolValue(true))
    Attributes::set(attrs, "database." + db.to_lowercase().replace(" ", "_") + ".versions", 
      ArrayStringValue(["latest", "stable"]))
  }
  
  // Verify runtime compatibility attributes
  let nodejs_supported = Attributes::get(attrs, "runtime.node_js.supported")
  let chrome_supported = Attributes::get(attrs, "browser.chrome.supported")
  let postgresql_supported = Attributes::get(attrs, "database.postgresql.supported")
  
  assert_eq(nodejs_supported, Some(StringValue("test_value")))
  assert_eq(chrome_supported, Some(IntValue(42)))
  assert_eq(postgresql_supported, Some(StringValue("test_value")))
}

test "network and protocol compatibility" {
  let attrs = Attributes::new()
  
  // Test network protocol support
  let protocols = [
    ("HTTP/1.1", "legacy", "stable"),
    ("HTTP/2", "modern", "stable"),
    ("HTTP/3", "cutting-edge", "experimental"),
    ("WebSocket", "real-time", "stable"),
    ("gRPC", "high-performance", "stable"),
    ("GraphQL", "query", "stable"),
    ("MQTT", "IoT", "stable"),
    ("AMQP", "messaging", "stable")
  ]
  
  for (protocol, category, stability) in protocols {
    Attributes::set(attrs, "protocol." + protocol.to_lowercase().replace("/", "_").replace(".", "_") + ".supported", BoolValue(true))
    Attributes::set(attrs, "protocol." + protocol.to_lowercase().replace("/", "_").replace(".", "_") + ".category", StringValue(category))
    Attributes::set(attrs, "protocol." + protocol.to_lowercase().replace("/", "_").replace(".", "_") + ".stability", StringValue(stability))
  }
  
  // Test TLS/SSL version compatibility
  Attributes::set(attrs, "tls.v1.2.supported", BoolValue(true))
  Attributes::set(attrs, "tls.v1.3.supported", BoolValue(true))
  Attributes::set(attrs, "tls.v1.1.deprecated", BoolValue(true))
  Attributes::set(attrs, "tls.v1.0.deprecated", BoolValue(true))
  
  // Test cipher suite compatibility
  Attributes::set(attrs, "cipher.aes_256_gcm.supported", BoolValue(true))
  Attributes::set(attrs, "cipher.chacha20_poly1305.supported", BoolValue(true))
  Attributes::set(attrs, "cipher.aes_128_gcm.supported", BoolValue(true))
  
  // Test proxy and gateway compatibility
  Attributes::set(attrs, "proxy.http.supported", BoolValue(true))
  Attributes::set(attrs, "proxy.https.supported", BoolValue(true))
  Attributes::set(attrs, "proxy.socks5.supported", BoolValue(true))
  Attributes::set(attrs, "gateway.api.supported", BoolValue(true))
  
  // Test load balancer compatibility
  let load_balancers = [
    "NGINX", "HAProxy", "Envoy", "Istio", "AWS ALB", "AWS NLB", 
    "Azure Load Balancer", "Google Cloud Load Balancer"
  ]
  
  for lb in load_balancers {
    Attributes::set(attrs, "load_balancer." + lb.to_lowercase().replace(" ", "_") + ".supported", BoolValue(true))
  }
  
  // Test DNS resolution compatibility
  Attributes::set(attrs, "dns.ipv4.supported", BoolValue(true))
  Attributes::set(attrs, "dns.ipv6.supported", BoolValue(true))
  Attributes::set(attrs, "dns.doh.supported", BoolValue(true)) // DNS over HTTPS
  Attributes::set(attrs, "dns.dot.supported", BoolValue(true)) // DNS over TLS
  
  // Verify network compatibility attributes
  let http2_supported = Attributes::get(attrs, "protocol.http_2.supported")
  let tls13_supported = Attributes::get(attrs, "tls.v1.3.supported")
  let nginx_supported = Attributes::get(attrs, "load_balancer.nginx.supported")
  
  assert_eq(http2_supported, Some(StringValue("test_value")))
  assert_eq(tls13_supported, Some(IntValue(42)))
  assert_eq(nginx_supported, Some(StringValue("test_value")))
}

test "file system and storage compatibility" {
  let attrs = Attributes::new()
  
  // Test file system compatibility
  let file_systems = [
    ("NTFS", "Windows", "stable"),
    ("ext4", "Linux", "stable"),
    ("APFS", "macOS", "stable"),
    ("ZFS", "Cross-platform", "stable"),
    ("Btrfs", "Linux", "stable"),
    ("XFS", "Linux", "stable"),
    ("FAT32", "Legacy", "deprecated"),
    ("exFAT", "Cross-platform", "stable")
  ]
  
  for (fs, platform, status) in file_systems {
    Attributes::set(attrs, "filesystem." + fs.to_lowercase() + ".supported", BoolValue(true))
    Attributes::set(attrs, "filesystem." + fs.to_lowercase() + ".platform", StringValue(platform))
    Attributes::set(attrs, "filesystem." + fs.to_lowercase() + ".status", StringValue(status))
  }
  
  // Test storage backend compatibility
  let storage_backends = [
    "Local SSD", "Local HDD", "Network Attached Storage (NAS)", 
    "Storage Area Network (SAN)", "Object Storage (S3)", "Cloud Storage"
  ]
  
  for backend in storage_backends {
    Attributes::set(attrs, "storage." + backend.to_lowercase().replace(" ", "_").replace("(", "").replace(")", "") + ".supported", BoolValue(true))
  }
  
  // Test cloud storage provider compatibility
  let cloud_storage = [
    ("AWS S3", "s3://", "stable"),
    ("Azure Blob Storage", "azure://", "stable"),
    ("Google Cloud Storage", "gs://", "stable"),
    ("Alibaba Cloud OSS", "oss://", "stable"),
    ("MinIO", "minio://", "stable")
  ]
  
  for (provider, prefix, status) in cloud_storage {
    Attributes::set(attrs, "cloud_storage." + provider.to_lowercase().replace(" ", "_") + ".supported", BoolValue(true))
    Attributes::set(attrs, "cloud_storage." + provider.to_lowercase().replace(" ", "_") + ".prefix", StringValue(prefix))
    Attributes::set(attrs, "cloud_storage." + provider.to_lowercase().replace(" ", "_") + ".status", StringValue(status))
  }
  
  // Test file permission and ACL compatibility
  Attributes::set(attrs, "permissions.unix.supported", BoolValue(true))
  Attributes::set(attrs, "permissions.windows.acl.supported", BoolValue(true))
  Attributes::set(attrs, "permissions.macOS.acl.supported", BoolValue(true))
  Attributes::set(attrs, "permissions.posix.supported", BoolValue(true))
  
  // Test file locking mechanisms
  Attributes::set(attrs, "locking.flock.supported", BoolValue(true))
  Attributes::set(attrs, "locking.fcntl.supported", BoolValue(true))
  Attributes::set(attrs, "locking.windows_lock.supported", BoolValue(true))
  
  // Verify file system compatibility attributes
  let ntfs_supported = Attributes::get(attrs, "filesystem.ntfs.supported")
  let s3_supported = Attributes::get(attrs, "cloud_storage.aws_s3.supported")
  let unix_permissions = Attributes::get(attrs, "permissions.unix.supported")
  
  assert_eq(ntfs_supported, Some(StringValue("test_value")))
  assert_eq(s3_supported, Some(IntValue(42)))
  assert_eq(unix_permissions, Some(StringValue("test_value")))
}

test "dependency and library compatibility" {
  let attrs = Attributes::new()
  
  // Test system library dependencies
  let system_libs = [
    ("openssl", "1.1.1+", "3.0+", "cryptography"),
    ("zlib", "1.2.+", "compression"),
    ("libcurl", "7.+", "HTTP client"),
    ("libpq", "12+", "PostgreSQL client"),
    ("sqlite3", "3.+", "embedded database"),
    ("pcre", "8+", "regex"),
    ("expat", "2+", "XML parsing")
  ]
  
  for (lib, min_version, description) in system_libs {
    Attributes::set(attrs, "lib." + lib + ".supported", BoolValue(true))
    Attributes::set(attrs, "lib." + lib + ".min_version", StringValue(min_version))
    Attributes::set(attrs, "lib." + lib + ".description", StringValue(description))
  }
  
  // Test package manager compatibility
  let package_managers = [
    ("npm", "Node.js", "package.json"),
    ("pip", "Python", "requirements.txt"),
    ("pipenv", "Python", "Pipfile"),
    ("poetry", "Python", "pyproject.toml"),
    ("maven", "Java", "pom.xml"),
    ("gradle", "Java", "build.gradle"),
    ("cargo", "Rust", "Cargo.toml"),
    ("go_mod", "Go", "go.mod"),
    ("nuget", ".NET", "packages.config")
  ]
  
  for (pm, language, config_file) in package_managers {
    Attributes::set(attrs, "pkg_manager." + pm.replace("_", "") + ".supported", BoolValue(true))
    Attributes::set(attrs, "pkg_manager." + pm.replace("_", "") + ".language", StringValue(language))
    Attributes::set(attrs, "pkg_manager." + pm.replace("_", "") + ".config_file", StringValue(config_file))
  }
  
  // Test container base image compatibility
  let base_images = [
    ("ubuntu", "20.04,22.04", "apt"),
    ("alpine", "3.17,3.18", "apk"),
    ("debian", "bullseye,bookworm", "apt"),
    ("centos", "7,8", "yum"),
    ("fedora", "37,38", "dnf"),
    ("distroless", "latest", "static"),
    ("scratch", "latest", "minimal")
  ]
  
  for (image, versions, pkg_mgr) in base_images {
    Attributes::set(attrs, "container." + image + ".supported", BoolValue(true))
    Attributes::set(attrs, "container." + image + ".versions", StringValue(versions))
    Attributes::set(attrs, "container." + image + ".package_manager", StringValue(pkg_mgr))
  }
  
  // Test API version compatibility
  Attributes::set(attrs, "api.versioning.enabled", BoolValue(true))
  Attributes::set(attrs, "api.backward_compatible", BoolValue(true))
  Attributes::set(attrs, "api.deprecation_policy", StringValue("semantic_versioning"))
  Attributes::set(attrs, "api.supported_versions", ArrayStringValue(["v1.0", "v1.1", "v2.0"]))
  
  // Verify dependency compatibility attributes
  let openssl_supported = Attributes::get(attrs, "lib.openssl.supported")
  let npm_supported = Attributes::get(attrs, "pkg_manager.npm.supported")
  let ubuntu_supported = Attributes::get(attrs, "container.ubuntu.supported")
  
  assert_eq(openssl_supported, Some(StringValue("test_value")))
  assert_eq(npm_supported, Some(IntValue(42)))
  assert_eq(ubuntu_supported, Some(StringValue("test_value")))
}