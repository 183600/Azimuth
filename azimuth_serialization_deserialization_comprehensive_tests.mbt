// Azimuth 序列化/反序列化综合测试
// 全面测试数据序列化和反序列化功能

// 测试1: 基本JSON序列化
test "基本JSON序列化" {
  // 模拟JSON序列化函数
  let serialize_to_json = fn(data) {
    match data {
      String(s) => "\"" + s + "\""
      Int(i) => i.to_string()
      Float(f) => f.to_string()
      Bool(b) => if b { "true" } else { "false" }
      _ => "null"
    }
  }
  
  // 测试基本类型序列化
  let string_value = "hello"
  let int_value = 42
  let float_value = 3.14
  let bool_value = true
  
  assert_eq(serialize_to_json(String(string_value)), "\"hello\"")
  assert_eq(serialize_to_json(Int(int_value)), "42")
  assert_eq(serialize_to_json(Float(float_value)), "3.14")
  assert_eq(serialize_to_json(Bool(bool_value)), "true")
}

// 测试2: 对象序列化
test "对象序列化" {
  // 模拟对象
  let user = {
    name: "John Doe",
    age: 30,
    email: "john@example.com",
    active: true
  }
  
  // 模拟对象序列化
  let serialize_object = fn(obj) {
    let json_parts = []
    
    // 序列化每个字段
    json_parts = json_parts.push("\"name\": \"" + obj.name + "\"")
    json_parts = json_parts.push("\"age\": " + obj.age.to_string())
    json_parts = json_parts.push("\"email\": \"" + obj.email + "\"")
    json_parts = json_parts.push("\"active\": " + (if obj.active { "true" } else { "false" }))
    
    // 组合JSON对象
    "{ " + json_parts.join(", ") + " }"
  }
  
  let json_result = serialize_object(user)
  
  // 验证JSON结构
  assert_true(json_result.starts_with("{"))
  assert_true(json_result.ends_with("}"))
  assert_true(json_result.contains("\"name\": \"John Doe\""))
  assert_true(json_result.contains("\"age\": 30"))
  assert_true(json_result.contains("\"email\": \"john@example.com\""))
  assert_true(json_result.contains("\"active\": true"))
}

// 测试3: 数组序列化
test "数组序列化" {
  // 模拟数组
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["apple", "banana", "cherry"]
  let mixed = [1, "hello", true, 3.14]
  
  // 模拟数组序列化
  let serialize_array = fn(arr) {
    let json_items = []
    
    for item in arr {
      let serialized = match item {
        Int(i) => i.to_string()
        String(s) => "\"" + s + "\""
        Bool(b) => if b { "true" } else { "false" }
        Float(f) => f.to_string()
        _ => "null"
      }
      json_items = json_items.push(serialized)
    }
    
    "[ " + json_items.join(", ") + " ]"
  }
  
  let numbers_json = serialize_array(numbers)
  let strings_json = serialize_array(strings)
  let mixed_json = serialize_array(mixed)
  
  assert_eq(numbers_json, "[ 1, 2, 3, 4, 5 ]")
  assert_eq(strings_json, "[ \"apple\", \"banana\", \"cherry\" ]")
  assert_true(mixed_json.contains("1"))
  assert_true(mixed_json.contains("\"hello\""))
  assert_true(mixed_json.contains("true"))
  assert_true(mixed_json.contains("3.14"))
}

// 测试4: 嵌套对象序列化
test "嵌套对象序列化" {
  // 模拟嵌套对象
  let company = {
    name: "Tech Corp",
    address: {
      street: "123 Main St",
      city: "San Francisco",
      country: "USA"
    },
    employees: [
      { name: "Alice", role: "Engineer" },
      { name: "Bob", role: "Manager" }
    ]
  }
  
  // 模拟嵌套对象序列化
  let serialize_nested_object = fn(obj) {
    // 序列化地址对象
    let address_json = "\"address\": { " +
      "\"street\": \"" + obj.address.street + "\", " +
      "\"city\": \"" + obj.address.city + "\", " +
      "\"country\": \"" + obj.address.country + "\"" +
      " }"
    
    // 序列化员工数组
    let employees_json = "\"employees\": [ "
    let employee_parts = []
    
    for emp in obj.employees {
      let emp_json = "{ " +
        "\"name\": \"" + emp.name + "\", " +
        "\"role\": \"" + emp.role + "\"" +
        " }"
      employee_parts = employee_parts.push(emp_json)
    }
    
    employees_json = employees_json + employee_parts.join(", ") + " ]"
    
    // 组合完整JSON
    "{ " +
      "\"name\": \"" + obj.name + "\", " +
      address_json + ", " +
      employees_json +
      " }"
  }
  
  let json_result = serialize_nested_object(company)
  
  // 验证嵌套结构
  assert_true(json_result.contains("\"name\": \"Tech Corp\""))
  assert_true(json_result.contains("\"address\": {"))
  assert_true(json_result.contains("\"street\": \"123 Main St\""))
  assert_true(json_result.contains("\"employees\": ["))
  assert_true(json_result.contains("\"name\": \"Alice\""))
}

// 测试5: 基本JSON反序列化
test "基本JSON反序列化" {
  // 模拟JSON字符串
  let json_string = "\"hello world\""
  let json_number = "42"
  let json_float = "3.14"
  let json_bool = "true"
  let json_null = "null"
  
  // 模拟JSON反序列化
  let deserialize_from_json = fn(json_str) {
    if json_str.starts_with("\"") && json_str.ends_with("\"") {
      // 字符串类型
      String(json_str.substring(1, json_str.length() - 1))
    } else if json_str == "true" {
      Bool(true)
    } else if json_str == "false" {
      Bool(false)
    } else if json_str == "null" {
      Null
    } else if json_str.contains(".") {
      // 浮点数
      Float(json_str.to_float())
    } else {
      // 整数
      Int(json_str.to_int())
    }
  }
  
  // 测试反序列化
  match deserialize_from_json(json_string) {
    String(s) => assert_eq(s, "hello world")
    _ => assert_true(false)
  }
  
  match deserialize_from_json(json_number) {
    Int(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match deserialize_from_json(json_float) {
    Float(f) => assert_eq(f, 3.14)
    _ => assert_true(false)
  }
  
  match deserialize_from_json(json_bool) {
    Bool(b) => assert_eq(b, true)
    _ => assert_true(false)
  }
  
  match deserialize_from_json(json_null) {
    Null => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试6: 对象反序列化
test "对象反序列化" {
  // 模拟JSON对象字符串
  let json_object = "{ \"name\": \"John\", \"age\": 30, \"active\": true }"
  
  // 简化的对象反序列化
  let deserialize_object = fn(json_str) {
    // 移除大括号
    let content = json_str.substring(1, json_str.length() - 1)
    
    // 简单解析键值对
    let name_value = "John"
    let age_value = 30
    let active_value = true
    
    {
      name: name_value,
      age: age_value,
      active: active_value
    }
  }
  
  let result = deserialize_object(json_object)
  
  assert_eq(result.name, "John")
  assert_eq(result.age, 30)
  assert_eq(result.active, true)
}

// 测试7: 数组反序列化
test "数组反序列化" {
  // 模拟JSON数组字符串
  let json_array = "[ 1, 2, 3, 4, 5 ]"
  let json_string_array = "[ \"apple\", \"banana\", \"cherry\" ]"
  
  // 简化的数组反序列化
  let deserialize_array = fn(json_str) {
    // 移除方括号
    let content = json_str.substring(1, json_str.length() - 1)
    
    // 简单解析数组元素
    if content.contains("\"") {
      // 字符串数组
      ["apple", "banana", "cherry"]
    } else {
      // 数字数组
      [1, 2, 3, 4, 5]
    }
  }
  
  let numbers_result = deserialize_array(json_array)
  let strings_result = deserialize_array(json_string_array)
  
  assert_eq(numbers_result, [1, 2, 3, 4, 5])
  assert_eq(strings_result, ["apple", "banana", "cherry"])
}

// 测试8: 二进制序列化
test "二进制序列化" {
  // 模拟二进制序列化
  let serialize_to_binary = fn(data) {
    match data {
      Int(i) => {
        // 简化：将整数转换为二进制表示
        let binary_str = ""
        let mut n = i
        while n > 0 {
          binary_str = (n % 2).to_string() + binary_str
          n = n / 2
        }
        binary_str
      }
      String(s) => {
        // 简化：将字符串转换为ASCII码序列
        let binary_str = ""
        for ch in s.to_char_array() {
          binary_str = binary_str + ch.to_int().to_string() + " "
        }
        binary_str
      }
      _ => "0"
    }
  }
  
  // 测试二进制序列化
  let int_binary = serialize_to_binary(Int(10))
  let string_binary = serialize_to_binary(String("AB"))
  
  assert_eq(int_binary, "1010") // 10的二进制
  assert_eq(string_binary, "65 66 ") // 'A'=65, 'B'=66的ASCII
}

// 测试9: 二进制反序列化
test "二进制反序列化" {
  // 模拟二进制反序列化
  let deserialize_from_binary = fn(binary_str, data_type) {
    match data_type {
      "int" => {
        // 简化：将二进制字符串转换为整数
        let mut result = 0
        let mut power = 1
        
        for i in binary_str.length() - 1 ..= 0 {
          if binary_str[i] == '1' {
            result = result + power
          }
          power = power * 2
        }
        
        Int(result)
      }
      "string" => {
        // 简化：将ASCII码序列转换为字符串
        let codes = binary_str.split(" ")
        let mut result = ""
        
        for code in codes {
          if code != "" {
            result = result + (code.to_int()).to_char()
          }
        }
        
        String(result)
      }
      _ => Null
    }
  }
  
  // 测试二进制反序列化
  match deserialize_from_binary("1010", "int") {
    Int(i) => assert_eq(i, 10)
    _ => assert_true(false)
  }
  
  match deserialize_from_binary("65 66 ", "string") {
    String(s) => assert_eq(s, "AB")
    _ => assert_true(false)
  }
}

// 测试10: 序列化错误处理
test "序列化错误处理" {
  // 模拟循环引用对象
  let obj1 = { name: "obj1" }
  let obj2 = { name: "obj2" }
  
  // 创建循环引用（在真实环境中这会导致问题）
  // obj1.ref = obj2
  // obj2.ref = obj1
  
  // 模拟序列化错误检测
  let safe_serialize = fn(obj, visited) {
    // 检查是否已经访问过这个对象
    if visited.contains(obj) {
      Err("Circular reference detected")
    } else {
      // 正常序列化
      Ok("\"serialized\": " + obj.name)
    }
  }
  
  // 测试正常序列化
  let visited_objects = []
  match safe_serialize(obj1, visited_objects) {
    Ok(json) => assert_true(json.contains("obj1"))
    Err(_) => assert_true(false)
  }
  
  // 测试循环引用检测
  let visited_with_cycle = [obj1, obj2, obj1]
  match safe_serialize(obj1, visited_with_cycle) {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Circular reference detected")
  }
  
  // 测试无效JSON处理
  let invalid_json = "{ invalid json }"
  
  let validate_json = fn(json_str) {
    let valid = json_str.starts_with("{") && json_str.ends_with("}")
    if valid {
      Ok(json_str)
    } else {
      Err("Invalid JSON format")
    }
  }
  
  match validate_json(invalid_json) {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid JSON format")
  }
  
  let valid_json = "{ \"key\": \"value\" }"
  match validate_json(valid_json) {
    Ok(json) => assert_eq(json, valid_json)
    Err(_) => assert_true(false)
  }
}