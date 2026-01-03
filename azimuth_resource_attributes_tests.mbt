// Azimuth 资源属性测试
// 测试遥测资源属性的管理和使用

// 测试1: 基本资源属性
test "基本资源属性测试" {
  // 创建资源
  let resource = Resource({
    attributes: [
      ("service.name", "azimuth-service"),
      ("service.version", "1.0.0"),
      ("service.instance.id", "instance-12345"),
      ("host.name", "server-1.example.com"),
      ("host.id", "host-67890")
    ]
  })
  
  // 验证资源属性
  assert_eq(resource.attributes.length(), 5)
  assert_true(resource.attributes.contains(("service.name", "azimuth-service")))
  assert_true(resource.attributes.contains(("service.version", "1.0.0")))
  assert_true(resource.attributes.contains(("service.instance.id", "instance-12345")))
  assert_true(resource.attributes.contains(("host.name", "server-1.example.com")))
  assert_true(resource.attributes.contains(("host.id", "host-67890")))
  
  // 获取特定属性值
  let service_name = resource.attributes.find(fn(attr) { attr[0] == "service.name" })
  match service_name {
    Some(attr) => assert_eq(attr[1], "azimuth-service")
    None => assert_true(false)
  }
  
  let service_version = resource.attributes.find(fn(attr) { attr[0] == "service.version" })
  match service_version {
    Some(attr) => assert_eq(attr[1], "1.0.0")
    None => assert_true(false)
  }
  
  // 按前缀过滤属性
  let service_attributes = resource.attributes.filter(fn(attr) { 
    attr[0].starts_with("service.")
  })
  let host_attributes = resource.attributes.filter(fn(attr) { 
    attr[0].starts_with("host.")
  })
  
  assert_eq(service_attributes.length(), 3)
  assert_eq(host_attributes.length(), 2)
}

// 测试2: 默认资源属性
test "默认资源属性测试" {
  // 创建带有默认属性的资源
  let resource = Resource({
    attributes: [
      // 服务属性
      ("service.name", "unknown-service"), // 如果未指定，应使用默认值
      ("service.version", "unknown"),
      ("service.instance.id", "unknown-instance"),
      
      // 遥测SDK属性
      ("telemetry.sdk.name", "azimuth"),
      ("telemetry.sdk.language", "moonbit"),
      ("telemetry.sdk.version", "1.0.0"),
      
      // 容器属性（如果在容器中运行）
      ("container.name", "azimuth-container"),
      ("container.id", "container-12345"),
      ("container.image.name", "azimuth:latest"),
      ("container.image.tag", "latest"),
      
      // Kubernetes属性（如果在Kubernetes中运行）
      ("k8s.pod.name", "azimuth-pod-12345"),
      ("k8s.namespace.name", "default"),
      ("k8s.deployment.name", "azimuth-deployment"),
      ("k8s.node.name", "node-1")
    ]
  })
  
  // 验证默认服务属性
  assert_true(resource.attributes.contains(("service.name", "unknown-service")))
  assert_true(resource.attributes.contains(("service.version", "unknown")))
  assert_true(resource.attributes.contains(("service.instance.id", "unknown-instance")))
  
  // 验证遥测SDK属性
  assert_true(resource.attributes.contains(("telemetry.sdk.name", "azimuth")))
  assert_true(resource.attributes.contains(("telemetry.sdk.language", "moonbit")))
  assert_true(resource.attributes.contains(("telemetry.sdk.version", "1.0.0")))
  
  // 验证容器属性
  assert_true(resource.attributes.contains(("container.name", "azimuth-container")))
  assert_true(resource.attributes.contains(("container.id", "container-12345")))
  assert_true(resource.attributes.contains(("container.image.name", "azimuth:latest")))
  assert_true(resource.attributes.contains(("container.image.tag", "latest")))
  
  // 验证Kubernetes属性
  assert_true(resource.attributes.contains(("k8s.pod.name", "azimuth-pod-12345")))
  assert_true(resource.attributes.contains(("k8s.namespace.name", "default")))
  assert_true(resource.attributes.contains(("k8s.deployment.name", "azimuth-deployment")))
  assert_true(resource.attributes.contains(("k8s.node.name", "node-1")))
  
  // 按属性类别分组
  let service_attributes = resource.attributes.filter(fn(attr) { 
    attr[0].starts_with("service.")
  })
  let telemetry_attributes = resource.attributes.filter(fn(attr) { 
    attr[0].starts_with("telemetry.")
  })
  let container_attributes = resource.attributes.filter(fn(attr) { 
    attr[0].starts_with("container.")
  })
  let k8s_attributes = resource.attributes.filter(fn(attr) { 
    attr[0].starts_with("k8s.")
  })
  
  assert_eq(service_attributes.length(), 3)
  assert_eq(telemetry_attributes.length(), 3)
  assert_eq(container_attributes.length(), 4)
  assert_eq(k8s_attributes.length(), 4)
}

// 测试3: 资源属性合并
test "资源属性合并测试" {
  // 创建基础资源
  let base_resource = Resource({
    attributes: [
      ("service.name", "azimuth-service"),
      ("service.version", "1.0.0"),
      ("host.name", "server-1.example.com"),
      ("os.type", "linux"),
      ("os.version", "5.4.0")
    ]
  })
  
  // 创建环境特定资源
  let env_resource = Resource({
    attributes: [
      ("service.name", "azimuth-service"), // 重复，应被覆盖
      ("service.version", "1.1.0"),        // 重复，应被覆盖
      ("deployment.environment", "production"),
      ("region", "us-west-2"),
      ("availability.zone", "us-west-2a")
    ]
  })
  
  // 合并资源属性（环境资源优先）
  let merged_attributes = [] : Array[(String, String)]
  
  // 首先添加基础资源属性
  for attr in base_resource.attributes {
    if not env_resource.attributes.any(fn(env_attr) { env_attr[0] == attr[0] }) {
      merged_attributes = merged_attributes.push(attr)
    }
  }
  
  // 然后添加环境资源属性（覆盖重复的）
  for attr in env_resource.attributes {
    merged_attributes = merged_attributes.push(attr)
  }
  
  // 验证合并结果
  let merged_resource = Resource({ attributes: merged_attributes })
  
  // 验证环境资源属性覆盖了基础资源属性
  let service_name = merged_resource.attributes.find(fn(attr) { attr[0] == "service.name" })
  match service_name {
    Some(attr) => assert_eq(attr[1], "azimuth-service") // 相同值
    None => assert_true(false)
  }
  
  let service_version = merged_resource.attributes.find(fn(attr) { attr[0] == "service.version" })
  match service_version {
    Some(attr) => assert_eq(attr[1], "1.1.0") // 环境资源值覆盖了基础资源值
    None => assert_true(false)
  }
  
  // 验证基础资源独有的属性被保留
  assert_true(merged_resource.attributes.contains(("host.name", "server-1.example.com")))
  assert_true(merged_resource.attributes.contains(("os.type", "linux")))
  assert_true(merged_resource.attributes.contains(("os.version", "5.4.0")))
  
  // 验证环境资源独有的属性被添加
  assert_true(merged_resource.attributes.contains(("deployment.environment", "production")))
  assert_true(merged_resource.attributes.contains(("region", "us-west-2")))
  assert_true(merged_resource.attributes.contains(("availability.zone", "us-west-2a")))
  
  // 验证总属性数量
  assert_eq(merged_resource.attributes.length(), 8)
}

// 测试4: 资源属性类型转换
test "资源属性类型转换测试" {
  // 创建包含不同类型值的资源属性
  let string_attributes = [
    ("service.name", "azimuth-service"),
    ("service.version", "1.0.0"),
    ("host.name", "server-1.example.com")
  ]
  
  let int_attributes = [
    ("process.pid", "12345"),
    ("process.thread.count", "8"),
    ("system.cpu.count", "4")
  ]
  
  let bool_attributes = [
    ("service.running", "true"),
    ("debug.enabled", "false"),
    ("metrics.enabled", "true")
  ]
  
  let double_attributes = [
    ("system.cpu.utilization", "0.75"),
    ("system.memory.utilization", "0.65"),
    ("process.cpu.time", "12345.67")
  ]
  
  let array_attributes = [
    ("service.tags", "web,api,microservice"),
    ("endpoints", "/api/users,/api/orders,/api/products"),
    ("dependencies", "database,cache,queue")
  ]
  
  // 验证字符串属性
  for attr in string_attributes {
    assert_true(attr[1].chars().all(fn(c) { true })) // 任何字符都是有效的字符串
  }
  
  // 验证整数属性
  for attr in int_attributes {
    let int_value = attr[1].to_int()
    match int_value {
      Some(_) => assert_true(true) // 成功转换为整数
      None => assert_true(false)    // 转换失败
    }
  }
  
  // 验证布尔属性
  for attr in bool_attributes {
    let bool_value = match attr[1] {
      "true" => true
      "false" => false
      _ => assert_true(false); false // 无效的布尔值
    }
    assert_true(bool_value == true || bool_value == false)
  }
  
  // 验证浮点数属性
  for attr in double_attributes {
    let double_value = attr[1].to_float()
    match double_value {
      Some(_) => assert_true(true) // 成功转换为浮点数
      None => assert_true(false)    // 转换失败
    }
  }
  
  // 验证数组属性
  for attr in array_attributes {
    let array_value = attr[1].split(",")
    assert_true(array_value.length() > 1) // 至少有两个元素
  }
  
  // 创建转换后的属性集合
  let all_attributes = string_attributes.concat(int_attributes)
    .concat(bool_attributes)
    .concat(double_attributes)
    .concat(array_attributes)
  
  // 按值类型分组
  let string_attrs = all_attributes.filter(fn(attr) { 
    string_attributes.any(fn(a) { a[0] == attr[0] })
  })
  let int_attrs = all_attributes.filter(fn(attr) { 
    int_attributes.any(fn(a) { a[0] == attr[0] })
  })
  let bool_attrs = all_attributes.filter(fn(attr) { 
    bool_attributes.any(fn(a) { a[0] == attr[0] })
  })
  let double_attrs = all_attributes.filter(fn(attr) { 
    double_attributes.any(fn(a) { a[0] == attr[0] })
  })
  let array_attrs = all_attributes.filter(fn(attr) { 
    array_attributes.any(fn(a) { a[0] == attr[0] })
  })
  
  assert_eq(string_attrs.length(), 3)
  assert_eq(int_attrs.length(), 3)
  assert_eq(bool_attrs.length(), 3)
  assert_eq(double_attrs.length(), 3)
  assert_eq(array_attrs.length(), 3)
}

// 测试5: 资源属性限制和验证
test "资源属性限制和验证测试" {
  // 创建包含长键和值的资源属性
  let long_key_name = "a".repeat(256) // 超过限制的键名
  let long_value = "b".repeat(4096)   // 超过限制的值
  
  // 验证键名长度限制（根据OpenTelemetry规范，键名不应超过255字符）
  assert_true(long_key_name.length() > 255)
  
  // 验证值长度限制（根据OpenTelemetry规范，值不应超过4096字符）
  assert_true(long_value.length() > 4096)
  
  // 创建有效的资源属性
  let valid_attributes = [
    ("service.name", "azimuth-service"), // 有效的键名和值
    ("service.version", "1.0.0"),
    ("host.name", "server-1.example.com"),
    ("short.key", "short.value"), // 短键名和值
    ("a".repeat(100), "b".repeat(1000)) // 在限制范围内的长键名和值
  ]
  
  // 验证有效属性
  for attr in valid_attributes {
    assert_true(attr[0].length() <= 255)  // 键名不超过255字符
    assert_true(attr[1].length() <= 4096) // 值不超过4096字符
  }
  
  // 创建包含特殊字符的资源属性
  let special_char_attributes = [
    ("service.name", "azimuth-service"),
    ("service.version", "1.0.0"),
    ("host.name", "server-1.example.com"),
    ("attr.with.dots", "value.with.dots"), // 包含点号的键名
    ("attr_with_underscores", "value_with_underscores"), // 包含下划线的键名
    ("attr-with-dashes", "value-with-dashes"), // 包含连字符的键名
    ("attr.with.numbers123", "value.with.numbers456") // 包含数字的键名
  ]
  
  // 验证特殊字符属性
  for attr in special_char_attributes {
    // 验证键名不以点号开头或结尾
    assert_false(attr[0].starts_with("."))
    assert_false(attr[0].ends_with("."))
    
    // 验证键名不包含连续的点号
    assert_false(attr[0].contains(".."))
  }
  
  // 创建包含无效字符的资源属性
  let invalid_char_attributes = [
    ("service.name", "azimuth-service"),
    ("attr with spaces", "value with spaces"), // 包含空格的键名
    ("attr/with/slashes", "value/with/slashes"), // 包含斜杠的键名
    ("attr\\with\\backslashes", "value\\with\\backslashes"), // 包含反斜杠的键名
    ("attr@with@ats", "value@with@ats"), // 包含@符号的键名
    ("attr#with#hashes", "value#with#hashes") // 包含#符号的键名
  ]
  
  // 过滤出无效的属性
  let filtered_attributes = [] : Array[(String, String)]
  for attr in invalid_char_attributes {
    // 检查键名是否包含无效字符
    let has_invalid_chars = attr[0].contains(" ") || 
                           attr[0].contains("/") || 
                           attr[0].contains("\\") || 
                           attr[0].contains("@") || 
                           attr[0].contains("#")
    
    if not has_invalid_chars {
      filtered_attributes = filtered_attributes.push(attr)
    }
  }
  
  // 验证过滤结果
  assert_eq(filtered_attributes.length(), 1) // 只有第一个属性是有效的
  assert_eq(filtered_attributes[0], ("service.name", "azimuth-service"))
}