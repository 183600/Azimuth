// Azimuth 高级序列化和反序列化测试
// 专注于测试系统的数据序列化和反序列化能力

// 测试1: 基本数据类型序列化测试
test "基本数据类型序列化测试" {
  // 定义序列化结果类型
  type SerializedData {
    data_type : String
    value : String
  }
  
  // 序列化整数
  let serialize_int = fn(value : Int) -> SerializedData {
    { data_type: "int", value: value.to_string() }
  }
  
  // 反序列化整数
  let deserialize_int = fn(data : SerializedData) -> Result[Int, String> {
    if data.data_type == "int" {
      match data.value.parse_int() {
        Some(value) => Ok(value)
        None => Err("Invalid integer format")
      }
    } else {
      Err("Type mismatch: expected int")
    }
  }
  
  // 测试整数序列化
  let int_value = 42
  let serialized_int = serialize_int(int_value)
  assert_eq(serialized_int.data_type, "int")
  assert_eq(serialized_int.value, "42")
  
  // 测试整数反序列化
  let deserialized_int = deserialize_int(serialized_int)
  match deserialized_int {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  // 序列化浮点数
  let serialize_float = fn(value : Double) -> SerializedData {
    { data_type: "float", value: value.to_string() }
  }
  
  // 反序列化浮点数
  let deserialize_float = fn(data : SerializedData) -> Result[Double, String> {
    if data.data_type == "float" {
      match data.value.parse_float() {
        Some(value) => Ok(value)
        None => Err("Invalid float format")
      }
    } else {
      Err("Type mismatch: expected float")
    }
  }
  
  // 测试浮点数序列化
  let float_value = 3.14159
  let serialized_float = serialize_float(float_value)
  assert_eq(serialized_float.data_type, "float")
  
  // 测试浮点数反序列化
  let deserialized_float = deserialize_float(serialized_float)
  match deserialized_float {
    Ok(value) => assert_true(abs(value - 3.14159) < 0.00001)
    Err(_) => assert_true(false)
  }
  
  // 序列化布尔值
  let serialize_bool = fn(value : Bool) -> SerializedData {
    { data_type: "bool", value: if value { "true" } else { "false" } }
  }
  
  // 反序列化布尔值
  let deserialize_bool = fn(data : SerializedData) -> Result[Bool, String] {
    if data.data_type == "bool" {
      if data.value == "true" { Ok(true) }
      else if data.value == "false" { Ok(false) }
      else { Err("Invalid boolean format") }
    } else {
      Err("Type mismatch: expected bool")
    }
  }
  
  // 测试布尔值序列化
  let bool_value = true
  let serialized_bool = serialize_bool(bool_value)
  assert_eq(serialized_bool.data_type, "bool")
  assert_eq(serialized_bool.value, "true")
  
  // 测试布尔值反序列化
  let deserialized_bool = deserialize_bool(serialized_bool)
  match deserialized_bool {
    Ok(value) => assert_eq(value, true)
    Err(_) => assert_true(false)
  }
}

// 测试2: 复杂数据结构序列化测试
test "复杂数据结构序列化测试" {
  // 定义记录类型
  type Person {
    name : String
    age : Int
    email : String
  }
  
  // 序列化Person记录
  let serialize_person = fn(person : Person) -> String {
    "{name:" + person.name + ",age:" + person.age.to_string() + ",email:" + person.email + "}"
  }
  
  // 反序列化Person记录
  let deserialize_person = fn(data : String) -> Result[Person, String> {
    // 简化的解析逻辑，实际实现会更复杂
    if data.contains("name:") && data.contains("age:") && data.contains("email:") {
      // 提取name
      let name_start = data.find("name:") + 5
      let name_end = data.find(",", name_start)
      let name = data.slice(name_start, name_end)
      
      // 提取age
      let age_start = data.find("age:") + 4
      let age_end = data.find(",", age_start)
      let age_str = data.slice(age_start, age_end)
      let age = match age_str.parse_int() {
        Some(value) => value
        None => { return Err("Invalid age format") }
      }
      
      // 提取email
      let email_start = data.find("email:") + 6
      let email_end = data.find("}", email_start)
      let email = data.slice(email_start, email_end)
      
      Ok({ name: name, age: age, email: email })
    } else {
      Err("Invalid person format")
    }
  }
  
  // 测试Person序列化
  let person = { name: "John Doe", age: 30, email: "john@example.com" }
  let serialized_person = serialize_person(person)
  assert_true(serialized_person.contains("name:John Doe"))
  assert_true(serialized_person.contains("age:30"))
  assert_true(serialized_person.contains("email:john@example.com"))
  
  // 测试Person反序列化
  let deserialized_person = deserialize_person(serialized_person)
  match deserialized_person {
    Ok(p) => {
      assert_eq(p.name, "John Doe")
      assert_eq(p.age, 30)
      assert_eq(p.email, "john@example.com")
    }
    Err(_) => assert_true(false)
  }
}

// 测试3: 数组和列表序列化测试
test "数组和列表序列化测试" {
  // 序列化整数数组
  let serialize_int_array = fn(arr : Array[Int>) -> String {
    let rec serialize_array = fn(data : Array[Int>, index : Int, result : String) -> String {
      if index >= data.length() { result }
      else {
        let new_result = if result == "" { data[index].to_string() } else { result + "," + data[index].to_string() }
        serialize_array(data, index + 1, new_result)
      }
    }
    "[" + serialize_array(arr, 0, "") + "]"
  }
  
  // 反序列化整数数组
  let deserialize_int_array = fn(data : String) -> Result[Array[Int>, String] {
    if data.starts_with("[") && data.ends_with("]") {
      let content = data.slice(1, data.length() - 1)
      if content == "" { Ok([]) }
      else {
        let parts = content.split(",")
        let rec parse_parts = fn(strs : Array<String>, index : Int, result : Array<Int>) -> Result[Array[Int>, String] {
          if index >= strs.length() { Ok(result) }
          else {
            match strs[index].parse_int() {
              Some(value) => parse_parts(strs, index + 1, result.push(value))
              None => Err("Invalid integer format")
            }
          }
        }
        parse_parts(parts, 0, [])
      }
    } else {
      Err("Invalid array format")
    }
  }
  
  // 测试数组序列化
  let int_array = [1, 2, 3, 4, 5]
  let serialized_array = serialize_int_array(int_array)
  assert_eq(serialized_array, "[1,2,3,4,5]")
  
  // 测试数组反序列化
  let deserialized_array = deserialize_int_array(serialized_array)
  match deserialized_array {
    Ok(arr) => assert_eq(arr, [1, 2, 3, 4, 5])
    Err(_) => assert_true(false)
  }
  
  // 测试空数组
  let empty_array = [] : Array[Int>
  let serialized_empty = serialize_int_array(empty_array)
  assert_eq(serialized_empty, "[]")
  
  let deserialized_empty = deserialize_int_array(serialized_empty)
  match deserialized_empty {
    Ok(arr) => assert_eq(arr.length(), 0)
    Err(_) => assert_true(false)
  }
}

// 测试4: 键值对序列化测试
test "键值对序列化测试" {
  // 序列化字典
  let serialize_dict = fn(dict : Map<String, String>) -> String {
    let rec serialize_pairs = fn(pairs : Array<(String, String)>, index : Int, result : String) -> String {
      if index >= pairs.length() { result }
      else {
        let (key, value) = pairs[index]
        let new_result = if result == "" { key + ":" + value } else { result + "," + key + ":" + value }
        serialize_pairs(pairs, index + 1, new_result)
      }
    }
    let pairs = dict.to_array()
    "{" + serialize_pairs(pairs, 0, "") + "}"
  }
  
  // 反序列化字典
  let deserialize_dict = fn(data : String) -> Result[Map[String, String>, String] {
    if data.starts_with("{") && data.ends_with("}") {
      let content = data.slice(1, data.length() - 1)
      if content == "" { Ok({}) }
      else {
        let pairs = content.split(",")
        let rec parse_pairs = fn(strs : Array<String>, index : Int, result : Map[String, String>) -> Result[Map[String, String], String] {
          if index >= strs.length() { Ok(result) }
          else {
            let pair = strs[index]
            match pair.find(":") {
              Some(colon_pos) => {
                let key = pair.slice(0, colon_pos)
                let value = pair.slice(colon_pos + 1, pair.length())
                parse_pairs(strs, index + 1, result.insert(key, value))
              }
              None => Err("Invalid key-value format")
            }
          }
        }
        parse_pairs(pairs, 0, {})
      }
    } else {
      Err("Invalid dictionary format")
    }
  }
  
  // 测试字典序列化
  let test_dict = {"name": "Alice", "age": "25", "city": "New York"}
  let serialized_dict = serialize_dict(test_dict)
  assert_true(serialized_dict.contains("name:Alice"))
  assert_true(serialized_dict.contains("age:25"))
  assert_true(serialized_dict.contains("city:New York"))
  
  // 测试字典反序列化
  let deserialized_dict = deserialize_dict(serialized_dict)
  match deserialized_dict {
    Ok(dict) => {
      match dict.get("name") {
        Some(value) => assert_eq(value, "Alice")
        None => assert_true(false)
      }
      match dict.get("age") {
        Some(value) => assert_eq(value, "25")
        None => assert_true(false)
      }
      match dict.get("city") {
        Some(value) => assert_eq(value, "New York")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// 测试5: 嵌套数据结构序列化测试
test "嵌套数据结构序列化测试" {
  // 定义嵌套结构
  type Address {
    street : String
    city : String
    country : String
  }
  
  type PersonWithAddress {
    name : String
    age : Int
    address : Address
  }
  
  // 序列化Address
  let serialize_address = fn(addr : Address) -> String {
    "{street:" + addr.street + ",city:" + addr.city + ",country:" + addr.country + "}"
  }
  
  // 序列化PersonWithAddress
  let serialize_person_with_address = fn(person : PersonWithAddress) -> String {
    let addr_str = serialize_address(person.address)
    "{name:" + person.name + ",age:" + person.age.to_string() + ",address:" + addr_str + "}"
  }
  
  // 反序列化Address
  let deserialize_address = fn(data : String) -> Result[Address, String> {
    if data.contains("street:") && data.contains("city:") && data.contains("country:") {
      // 简化的解析逻辑
      let street_start = data.find("street:") + 7
      let street_end = data.find(",", street_start)
      let street = data.slice(street_start, street_end)
      
      let city_start = data.find("city:") + 5
      let city_end = data.find(",", city_start)
      let city = data.slice(city_start, city_end)
      
      let country_start = data.find("country:") + 8
      let country_end = data.find("}", country_start)
      let country = data.slice(country_start, country_end)
      
      Ok({ street: street, city: city, country: country })
    } else {
      Err("Invalid address format")
    }
  }
  
  // 反序列化PersonWithAddress
  let deserialize_person_with_address = fn(data : String) -> Result[PersonWithAddress, String> {
    if data.contains("name:") && data.contains("age:") && data.contains("address:") {
      // 提取name
      let name_start = data.find("name:") + 5
      let name_end = data.find(",", name_start)
      let name = data.slice(name_start, name_end)
      
      // 提取age
      let age_start = data.find("age:") + 4
      let age_end = data.find(",", age_start)
      let age_str = data.slice(age_start, age_end)
      let age = match age_str.parse_int() {
        Some(value) => value
        None => { return Err("Invalid age format") }
      }
      
      // 提取address
      let addr_start = data.find("address:") + 8
      let addr_data = data.slice(addr_start, data.length())
      let address = match deserialize_address(addr_data) {
        Ok(addr) => addr
        Err(err) => { return Err(err) }
      }
      
      Ok({ name: name, age: age, address: address })
    } else {
      Err("Invalid person with address format")
    }
  }
  
  // 测试嵌套结构序列化
  let address = { street: "123 Main St", city: "Boston", country: "USA" }
  let person = { name: "Bob Smith", age: 35, address: address }
  let serialized_person = serialize_person_with_address(person)
  
  assert_true(serialized_person.contains("name:Bob Smith"))
  assert_true(serialized_person.contains("age:35"))
  assert_true(serialized_person.contains("street:123 Main St"))
  assert_true(serialized_person.contains("city:Boston"))
  assert_true(serialized_person.contains("country:USA"))
  
  // 测试嵌套结构反序列化
  let deserialized_person = deserialize_person_with_address(serialized_person)
  match deserialized_person {
    Ok(p) => {
      assert_eq(p.name, "Bob Smith")
      assert_eq(p.age, 35)
      assert_eq(p.address.street, "123 Main St")
      assert_eq(p.address.city, "Boston")
      assert_eq(p.address.country, "USA")
    }
    Err(_) => assert_true(false)
  }
}

// 测试6: 序列化错误处理测试
test "序列化错误处理测试" {
  // 定义序列化错误类型
  type SerializationError {
    InvalidFormat(String)
    TypeMismatch(String)
    DataCorruption(String)
  }
  
  // 安全的整数反序列化
  let safe_deserialize_int = fn(data : String) -> Result[Int, SerializationError] {
    if data.is_empty() {
      Err(InvalidFormat("Empty string"))
    } else if data.contains(".") {
      Err(TypeMismatch("Expected integer, got float"))
    } else {
      match data.parse_int() {
        Some(value) => Ok(value)
        None => Err(InvalidFormat("Cannot parse as integer"))
      }
    }
  }
  
  // 测试错误处理
  let result1 = safe_deserialize_int("123")
  match result1 {
    Ok(value) => assert_eq(value, 123)
    Err(_) => assert_true(false)
  }
  
  let result2 = safe_deserialize_int("")
  match result2 {
    Ok(_) => assert_true(false)
    Err(InvalidFormat(msg)) => assert_eq(msg, "Empty string")
    Err(_) => assert_true(false)
  }
  
  let result3 = safe_deserialize_int("123.45")
  match result3 {
    Ok(_) => assert_true(false)
    Err(TypeMismatch(msg)) => assert_eq(msg, "Expected integer, got float")
    Err(_) => assert_true(false)
  }
  
  let result4 = safe_deserialize_int("abc")
  match result4 {
    Ok(_) => assert_true(false)
    Err(InvalidFormat(msg)) => assert_eq(msg, "Cannot parse as integer")
    Err(_) => assert_true(false)
  }
  
  // 测试错误恢复
  let recover_from_error = fn(result : Result[Int, SerializationError>) -> Int {
    match result {
      Ok(value) => value
      Err(_) => 0  // 默认值
    }
  }
  
  assert_eq(recover_from_error(result1), 123)
  assert_eq(recover_from_error(result2), 0)
  assert_eq(recover_from_error(result3), 0)
  assert_eq(recover_from_error(result4), 0)
}