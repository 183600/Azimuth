// Azimuth 数据序列化测试用例
// 专注于JSON、XML、二进制序列化和自定义格式处理功能测试

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化测试" {
  // 测试基本类型序列化
  let json_string = JsonSerializer::serialize(42)
  assert_eq(json_string, "42")
  
  let json_float = JsonSerializer::serialize(3.14)
  assert_eq(json_float, "3.14")
  
  let json_bool = JsonSerializer::serialize(true)
  assert_eq(json_bool, "true")
  
  let json_null = JsonSerializer::serialize(None)
  assert_eq(json_null, "null")
  
  let json_str = JsonSerializer::serialize("Hello, JSON!")
  assert_eq(json_str, "\"Hello, JSON!\"")
  
  // 测试数组序列化
  let array_data = [1, 2, 3, 4, 5]
  let json_array = JsonSerializer::serialize(array_data)
  assert_eq(json_array, "[1,2,3,4,5]")
  
  let string_array = ["apple", "banana", "cherry"]
  let json_string_array = JsonSerializer::serialize(string_array)
  assert_eq(json_string_array, "[\"apple\",\"banana\",\"cherry\"]")
  
  // 测试对象序列化
  let person = Person::new("Alice", 30, "alice@example.com")
  let json_object = JsonSerializer::serialize(person)
  assert_true(json_object.contains("\"name\":\"Alice\""))
  assert_true(json_object.contains("\"age\":30"))
  assert_true(json_object.contains("\"email\":\"alice@example.com\""))
  
  // 测试嵌套对象序列化
  let address = Address::new("123 Main St", "Anytown", "12345")
  let person_with_address = PersonWithAddress::new("Bob", 25, "bob@example.com", address)
  let json_nested = JsonSerializer::serialize(person_with_address)
  assert_true(json_nested.contains("\"name\":\"Bob\""))
  assert_true(json_nested.contains("\"address\""))
  assert_true(json_nested.contains("\"street\":\"123 Main St\""))
  
  // 测试反序列化
  let parsed_int = JsonSerializer::deserialize[Int]("42")
  assert_eq(parsed_int, 42)
  
  let parsed_float = JsonSerializer::deserialize[Float]("3.14")
  assert_true(parsed_float - 3.14 < 0.001)
  
  let parsed_bool = JsonSerializer::deserialize[Bool]("true")
  assert_eq(parsed_bool, true)
  
  let parsed_string = JsonSerializer::deserialize[String]("\"Hello, JSON!\"")
  assert_eq(parsed_string, "Hello, JSON!")
  
  let parsed_array = JsonSerializer::deserialize[Array[Int]]("[1,2,3,4,5]")
  assert_eq(parsed_array.length(), 5)
  assert_eq(parsed_array[0], 1)
  assert_eq(parsed_array[4], 5)
  
  let parsed_person = JsonSerializer::deserialize[Person](json_object)
  assert_eq(Person::name(parsed_person), "Alice")
  assert_eq(Person::age(parsed_person), 30)
  assert_eq(Person::email(parsed_person), "alice@example.com")
}

// 测试2: JSON高级特性
test "JSON高级特性测试" {
  // 测试可选字段和空值处理
  let person_with_optional = PersonWithOptional::new("Charlie", Some(35), None)
  let json_optional = JsonSerializer::serialize(person_with_optional)
  assert_true(json_optional.contains("\"name\":\"Charlie\""))
  assert_true(json_optional.contains("\"age\":35"))
  assert_false(json_optional.contains("\"email\"")) // None字段应该被省略
  
  // 测试自定义序列化
  let custom_date = CustomDate::new(2023, 12, 25)
  let json_custom = JsonSerializer::serialize_with_formatter(custom_date, fn(date) {
    date.year.to_string() + "-" + 
    (if date.month < 10 { "0" } else { "" }) + date.month.to_string() + "-" +
    (if date.day < 10 { "0" } else { "" }) + date.day.to_string()
  })
  assert_eq(json_custom, "\"2023-12-25\"")
  
  // 测试自定义反序列化
  let parsed_date = JsonSerializer::deserialize_with_parser[CustomDate]("\"2023-12-25\"", fn(date_str) {
    let parts = date_str.split("-")
    CustomDate::new(
      parts[0].to_int(),
      parts[1].to_int(),
      parts[2].to_int()
    )
  })
  assert_eq(CustomDate::year(parsed_date), 2023)
  assert_eq(CustomDate::month(parsed_date), 12)
  assert_eq(CustomDate::day(parsed_date), 25)
  
  // 测试JSON路径查询
  let complex_json = "{\"user\":{\"name\":\"Alice\",\"profile\":{\"age\":30,\"settings\":{\"theme\":\"dark\",\"notifications\":true}}}}"
  let name = JsonPath::query(complex_json, "user.name")
  assert_eq(name, "Alice")
  
  let age = JsonPath::query(complex_json, "user.profile.age")
  assert_eq(age, "30")
  
  let theme = JsonPath::query(complex_json, "user.profile.settings.theme")
  assert_eq(theme, "dark")
  
  let notifications = JsonPath::query(complex_json, "user.profile.settings.notifications")
  assert_eq(notifications, "true")
  
  // 测试JSON合并
  let json1 = "{\"a\":1,\"b\":2}"
  let json2 = "{\"b\":3,\"c\":4}"
  let merged = JsonUtils::merge(json1, json2)
  assert_true(merged.contains("\"a\":1"))
  assert_true(merged.contains("\"b\":3")) // json2的值应该覆盖json1
  assert_true(merged.contains("\"c\":4"))
  
  // 测试JSON验证
  let valid_json = "{\"name\":\"Alice\",\"age\":30}"
  assert_true(JsonValidator::is_valid(valid_json))
  
  let invalid_json = "{\"name\":\"Alice\",\"age\":30" // 缺少闭合括号
  assert_false(JsonValidator::is_valid(invalid_json))
  
  // 测试JSON格式化
  let minified_json = "{\"name\":\"Alice\",\"age\":30,\"email\":\"alice@example.com\"}"
  let formatted_json = JsonFormatter::pretty_print(minified_json, 2)
  assert_true(formatted_json.contains("\n"))
  assert_true(formatted_json.contains("  ")) // 缩进
}

// 测试3: XML序列化和反序列化
test "XML序列化和反序列化测试" {
  // 测试基本XML元素创建
  let element = XmlElement::new("person")
  XmlElement::set_attribute(element, "id", "123")
  XmlElement::add_child(element, XmlElement::new_text("name", "Alice"))
  XmlElement::add_child(element, XmlElement::new_text("age", "30"))
  
  // 测试XML序列化
  let xml_string = XmlSerializer::serialize(element)
  assert_true(xml_string.contains("<person id=\"123\">"))
  assert_true(xml_string.contains("<name>Alice</name>"))
  assert_true(xml_string.contains("<age>30</age>"))
  assert_true(xml_string.contains("</person>"))
  
  // 测试XML反序列化
  let parsed_element = XmlSerializer::deserialize(xml_string)
  assert_eq(XmlElement::name(parsed_element), "person")
  assert_eq(XmlElement::attribute(parsed_element, "id"), Some("123"))
  
  let name_element = XmlElement::child(parsed_element, "name")
  assert_eq(XmlElement::name(name_element), "name")
  assert_eq(XmlElement::text(name_element), "Alice")
  
  let age_element = XmlElement::child(parsed_element, "age")
  assert_eq(XmlElement::name(age_element), "age")
  assert_eq(XmlElement::text(age_element), "30")
  
  // 测试复杂XML结构
  let company = XmlElement::new("company")
  XmlElement::set_attribute(company, "name", "Tech Corp")
  
  let employees = XmlElement::new("employees")
  
  let employee1 = XmlElement::new("employee")
  XmlElement::set_attribute(employee1, "id", "1")
  XmlElement::add_child(employee1, XmlElement::new_text("name", "Alice"))
  XmlElement::add_child(employee1, XmlElement::new_text("department", "Engineering"))
  
  let employee2 = XmlElement::new("employee")
  XmlElement::set_attribute(employee2, "id", "2")
  XmlElement::add_child(employee2, XmlElement::new_text("name", "Bob"))
  XmlElement::add_child(employee2, XmlElement::new_text("department", "Marketing"))
  
  XmlElement::add_child(employees, employee1)
  XmlElement::add_child(employees, employee2)
  XmlElement::add_child(company, employees)
  
  let complex_xml = XmlSerializer::serialize(company)
  assert_true(complex_xml.contains("<company name=\"Tech Corp\">"))
  assert_true(complex_xml.contains("<employees>"))
  assert_true(complex_xml.contains("<employee id=\"1\">"))
  assert_true(complex_xml.contains("<name>Alice</name>"))
  assert_true(complex_xml.contains("<department>Engineering</department>"))
  
  // 测试XML命名空间
  let ns_element = XmlElement::new_with_namespace("item", "http://example.com/ns")
  XmlElement::set_attribute(ns_element, "xmlns", "http://example.com/ns")
  XmlElement::add_child(ns_element, XmlElement::new_text("title", "Sample Item"))
  
  let ns_xml = XmlSerializer::serialize(ns_element)
  assert_true(ns_xml.contains("xmlns=\"http://example.com/ns\""))
  
  // 测试XPath查询
  let xpath_result = XmlXPath::query(complex_xml, "//employee[name='Alice']/department")
  assert_eq(xpath_result, "Engineering")
  
  let all_names = XmlXPath::query_all(complex_xml, "//employee/name/text()")
  assert_eq(all_names.length(), 2)
  assert_true(all_names.contains("Alice"))
  assert_true(all_names.contains("Bob"))
  
  // 测试XML验证
  let valid_xml = "<root><child>content</child></root>"
  assert_true(XmlValidator::is_valid(valid_xml))
  
  let invalid_xml = "<root><child>content" // 缺少闭合标签
  assert_false(XmlValidator::is_valid(invalid_xml))
  
  // 测试XML转换
  let transformed_xml = XmlTransformer::transform(complex_xml, fn(element) {
    if XmlElement::name(element) == "employee" {
      XmlElement::set_attribute(element, "status", "active")
    }
    element
  })
  assert_true(transformed_xml.contains("status=\"active\""))
}

// 测试4: 二进制序列化
test "二进制序列化测试" {
  // 测试基本类型二进制序列化
  let binary_int = BinarySerializer::serialize(42)
  assert_eq(binary_int.length(), 4) // 32位整数 = 4字节
  
  let binary_float = BinarySerializer::serialize(3.14)
  assert_eq(binary_float.length(), 4) // 32位浮点数 = 4字节
  
  let binary_bool = BinarySerializer::serialize(true)
  assert_eq(binary_bool.length(), 1) // 布尔值 = 1字节
  
  let binary_string = BinarySerializer::serialize("Hello, Binary!")
  // 字符串 = 长度前缀(4字节) + 字符内容
  assert_eq(binary_string.length(), 4 + "Hello, Binary!".length())
  
  // 测试二进制反序列化
  let parsed_int = BinarySerializer::deserialize[Int](binary_int)
  assert_eq(parsed_int, 42)
  
  let parsed_float = BinarySerializer::deserialize[Float](binary_float)
  assert_true(parsed_float - 3.14 < 0.001)
  
  let parsed_bool = BinarySerializer::deserialize[Bool](binary_bool)
  assert_eq(parsed_bool, true)
  
  let parsed_string = BinarySerializer::deserialize[String](binary_string)
  assert_eq(parsed_string, "Hello, Binary!")
  
  // 测试复杂对象二进制序列化
  let person = Person::new("Alice", 30, "alice@example.com")
  let binary_person = BinarySerializer::serialize(person)
  
  let parsed_person = BinarySerializer::deserialize[Person](binary_person)
  assert_eq(Person::name(parsed_person), "Alice")
  assert_eq(Person::age(parsed_person), 30)
  assert_eq(Person::email(parsed_person), "alice@example.com")
  
  // 测试二进制序列化版本控制
  let versioned_serializer = BinarySerializer::with_version(1)
  let versioned_data = BinarySerializer::serialize_with_version(versioned_serializer, person)
  
  let versioned_deserializer = BinarySerializer::with_version(1)
  let versioned_parsed = BinarySerializer::deserialize_with_version[Person](versioned_deserializer, versioned_data)
  assert_eq(Person::name(versioned_parsed), "Alice")
  
  // 测试二进制压缩
  let large_data = "A".repeat(10000)
  let uncompressed = BinarySerializer::serialize(large_data)
  let compressed = BinarySerializer::compress(uncompressed)
  
  assert_true(compressed.length() < uncompressed.length())
  
  let decompressed = BinarySerializer::decompress(compressed)
  let parsed_large = BinarySerializer::deserialize[String](decompressed)
  assert_eq(parsed_large, large_data)
  
  // 测试二进制加密
  let encryption_key = "encryption-key-16"
  let encrypted = BinarySerializer::encrypt(uncompressed, encryption_key)
  let decrypted = BinarySerializer::decrypt(encrypted, encryption_key)
  
  assert_ne(encrypted, uncompressed)
  assert_eq(decrypted, uncompressed)
}

// 测试5: Protocol Buffers序列化
test "Protocol Buffers序列化测试" {
  // 测试Protocol Buffers消息定义
  let person_proto = ProtoMessage::new("Person")
  ProtoMessage::add_field(person_proto, "name", ProtoFieldType::String, 1)
  ProtoMessage::add_field(person_proto, "age", ProtoFieldType::Int32, 2)
  ProtoMessage::add_field(person_proto, "email", ProtoFieldType::String, 3)
  
  // 测试Protocol Buffers序列化
  let person = Person::new("Alice", 30, "alice@example.com")
  let proto_data = ProtoSerializer::serialize(person, person_proto)
  
  // Protocol Buffers数据应该是二进制格式
  assert_true(proto_data.length() > 0)
  
  // 测试Protocol Buffers反序列化
  let parsed_person = ProtoSerializer::deserialize[Person](proto_data, person_proto)
  assert_eq(Person::name(parsed_person), "Alice")
  assert_eq(Person::age(parsed_person), 30)
  assert_eq(Person::email(parsed_person), "alice@example.com")
  
  // 测试嵌套Protocol Buffers消息
  let address_proto = ProtoMessage::new("Address")
  ProtoMessage::add_field(address_proto, "street", ProtoFieldType::String, 1)
  ProtoMessage::add_field(address_proto, "city", ProtoFieldType::String, 2)
  ProtoMessage::add_field(address_proto, "zip", ProtoFieldType::String, 3)
  
  let person_with_address_proto = ProtoMessage::new("PersonWithAddress")
  ProtoMessage::add_field(person_with_address_proto, "name", ProtoFieldType::String, 1)
  ProtoMessage::add_field(person_with_address_proto, "age", ProtoFieldType::Int32, 2)
  ProtoMessage::add_field(person_with_address_proto, "address", ProtoFieldType::Message, 3, address_proto)
  
  let address = Address::new("123 Main St", "Anytown", "12345")
  let person_with_address = PersonWithAddress::new("Bob", 25, "bob@example.com", address)
  
  let nested_proto_data = ProtoSerializer::serialize(person_with_address, person_with_address_proto)
  let parsed_nested = ProtoSerializer::deserialize[PersonWithAddress](nested_proto_data, person_with_address_proto)
  
  assert_eq(PersonWithAddress::name(parsed_nested), "Bob")
  assert_eq(PersonWithAddress::age(parsed_nested), 25)
  assert_eq(Address::street(PersonWithAddress::address(parsed_nested)), "123 Main St")
  
  // 测试Protocol Buffers重复字段
  let list_proto = ProtoMessage::new("PersonList")
  ProtoMessage::add_field(list_proto, "people", ProtoFieldType::RepeatedMessage, 1, person_proto)
  
  let people = [person, parsed_person]
  let person_list = PersonList::new(people)
  let list_proto_data = ProtoSerializer::serialize(person_list, list_proto)
  
  let parsed_list = ProtoSerializer::deserialize[PersonList](list_proto_data, list_proto)
  assert_eq(PersonList::people(parsed_list).length(), 2)
  
  // 测试Protocol Buffers向前兼容性
  let extended_person_proto = ProtoMessage::new("ExtendedPerson")
  ProtoMessage::add_field(extended_person_proto, "name", ProtoFieldType::String, 1)
  ProtoMessage::add_field(extended_person_proto, "age", ProtoFieldType::Int32, 2)
  ProtoMessage::add_field(extended_person_proto, "email", ProtoFieldType::String, 3)
  ProtoMessage::add_field(extended_person_proto, "phone", ProtoFieldType::String, 4) // 新字段
  
  let extended_person = ExtendedPerson::new("Charlie", 35, "charlie@example.com", "555-1234")
  let extended_proto_data = ProtoSerializer::serialize(extended_person, extended_person_proto)
  
  // 使用旧的消息定义解析新数据（新字段应该被忽略）
  let backward_compatible = ProtoSerializer::deserialize[Person](extended_proto_data, person_proto)
  assert_eq(Person::name(backward_compatible), "Charlie")
  assert_eq(Person::age(backward_compatible), 35)
  assert_eq(Person::email(backward_compatible), "charlie@example.com")
}

// 测试6: 自定义序列化格式
test "自定义序列化格式测试" {
  // 测试CSV序列化
  let people = [
    Person::new("Alice", 30, "alice@example.com"),
    Person::new("Bob", 25, "bob@example.com"),
    Person::new("Charlie", 35, "charlie@example.com")
  ]
  
  let csv_data = CsvSerializer::serialize(people)
  let lines = csv_data.split("\n")
  assert_eq(lines.length(), 4) // 头部 + 3行数据
  assert_eq(lines[0], "name,age,email")
  assert_eq(lines[1], "Alice,30,alice@example.com")
  
  // 测试CSV反序列化
  let parsed_people = CsvSerializer::deserialize[Person](csv_data)
  assert_eq(parsed_people.length(), 3)
  assert_eq(Person::name(parsed_people[0]), "Alice")
  assert_eq(Person::age(parsed_people[1]), 25)
  
  // 测试YAML序列化
  let config = Config::new(
    "localhost",
    8080,
    DatabaseConfig::new("mydb", "user", "pass"),
    ["feature1", "feature2"]
  )
  
  let yaml_data = YamlSerializer::serialize(config)
  assert_true(yaml_data.contains("server:"))
  assert_true(yaml_data.contains("host: localhost"))
  assert_true(yaml_data.contains("port: 8080"))
  assert_true(yaml_data.contains("database:"))
  assert_true(yaml_data.contains("features:"))
  
  // 测试YAML反序列化
  let parsed_config = YamlSerializer::deserialize[Config](yaml_data)
  assert_eq(Config::host(parsed_config), "localhost")
  assert_eq(Config::port(parsed_config), 8080)
  assert_eq(DatabaseConfig::name(Config::database(parsed_config)), "mydb")
  assert_eq(Config::features(parsed_config).length(), 2)
  
  // 测试TOML序列化
  let toml_data = TomlSerializer::serialize(config)
  assert_true(toml_data.contains("[server]"))
  assert_true(toml_data.contains("host = \"localhost\""))
  assert_true(toml_data.contains("port = 8080"))
  
  // 测试TOML反序列化
  let parsed_toml_config = TomlSerializer::deserialize[Config](toml_data)
  assert_eq(Config::host(parsed_toml_config), "localhost")
  assert_eq(Config::port(parsed_toml_config), 8080)
  
  // 测试自定义序列化器
  let custom_serializer = CustomSerializer::new()
  CustomSerializer::register_formatter(custom_serializer, "Person", fn(person) {
    Person::name(person) + "|" + Person::age(person).to_string() + "|" + Person::email(person)
  })
  
  CustomSerializer::register_parser(custom_serializer, "Person", fn(data) {
    let parts = data.split("|")
    Person::new(parts[0], parts[1].to_int(), parts[2])
  })
  
  let custom_data = CustomSerializer::serialize(custom_serializer, person)
  assert_eq(custom_data, "Alice|30|alice@example.com")
  
  let custom_parsed = CustomSerializer::deserialize[Person](custom_serializer, custom_data)
  assert_eq(Person::name(custom_parsed), "Alice")
  assert_eq(Person::age(custom_parsed), 30)
}

// 测试7: 序列化性能测试
test "序列化性能测试" {
  // 创建大量测试数据
  let large_dataset = []
  for i in 1..=10000 {
    let person = Person::new(
      "Person" + i.to_string(),
      20 + (i % 50),
      "person" + i.to_string() + "@example.com"
    )
    large_dataset = large_dataset.push(person)
  }
  
  // 测试JSON序列化性能
  let start_time = Time::now()
  let json_data = JsonSerializer::serialize(large_dataset)
  let json_serialization_time = Time::elapsed_since(start_time)
  
  assert_true(json_data.length() > 0)
  
  // 测试JSON反序列化性能
  start_time = Time::now()
  let json_parsed = JsonSerializer::deserialize[Array[Person]](json_data)
  let json_deserialization_time = Time::elapsed_since(start_time)
  
  assert_eq(json_parsed.length(), 10000)
  
  // 测试二进制序列化性能
  start_time = Time::now()
  let binary_data = BinarySerializer::serialize(large_dataset)
  let binary_serialization_time = Time::elapsed_since(start_time)
  
  // 测试二进制反序列化性能
  start_time = Time::now()
  let binary_parsed = BinarySerializer::deserialize[Array[Person]](binary_data)
  let binary_deserialization_time = Time::elapsed_since(start_time)
  
  assert_eq(binary_parsed.length(), 10000)
  
  // 测试Protocol Buffers序列化性能
  let person_proto = ProtoMessage::new("Person")
  ProtoMessage::add_field(person_proto, "name", ProtoFieldType::String, 1)
  ProtoMessage::add_field(person_proto, "age", ProtoFieldType::Int32, 2)
  ProtoMessage::add_field(person_proto, "email", ProtoFieldType::String, 3)
  
  let list_proto = ProtoMessage::new("PersonList")
  ProtoMessage::add_field(list_proto, "people", ProtoFieldType::RepeatedMessage, 1, person_proto)
  
  let person_list = PersonList::new(large_dataset)
  
  start_time = Time::now()
  let proto_data = ProtoSerializer::serialize(person_list, list_proto)
  let proto_serialization_time = Time::elapsed_since(start_time)
  
  start_time = Time::now()
  let proto_parsed = ProtoSerializer::deserialize[PersonList](proto_data, list_proto)
  let proto_deserialization_time = Time::elapsed_since(start_time)
  
  assert_eq(PersonList::people(proto_parsed).length(), 10000)
  
  // 比较性能
  assert_true(json_serialization_time < 10000) // 应该在10秒内完成
  assert_true(json_deserialization_time < 10000) // 应该在10秒内完成
  assert_true(binary_serialization_time < 10000) // 应该在10秒内完成
  assert_true(binary_deserialization_time < 10000) // 应该在10秒内完成
  assert_true(proto_serialization_time < 10000) // 应该在10秒内完成
  assert_true(proto_deserialization_time < 10000) // 应该在10秒内完成
  
  // 比较数据大小
  assert_true(binary_data.length() < json_data.length()) // 二进制应该更小
  assert_true(proto_data.length() < json_data.length()) // Proto应该更小
  
  // 验证数据完整性
  for i in 0..10000 {
    let original = large_dataset[i]
    let json_result = json_parsed[i]
    let binary_result = binary_parsed[i]
    let proto_result = PersonList::people(proto_parsed)[i]
    
    assert_eq(Person::name(original), Person::name(json_result))
    assert_eq(Person::age(original), Person::age(json_result))
    assert_eq(Person::email(original), Person::email(json_result))
    
    assert_eq(Person::name(original), Person::name(binary_result))
    assert_eq(Person::age(original), Person::age(binary_result))
    assert_eq(Person::email(original), Person::email(binary_result))
    
    assert_eq(Person::name(original), Person::name(proto_result))
    assert_eq(Person::age(original), Person::age(proto_result))
    assert_eq(Person::email(original), Person::email(proto_result))
  }
}

// 测试8: 序列化错误处理
test "序列化错误处理测试" {
  // 测试JSON解析错误
  let malformed_json = "{\"name\":\"Alice\",\"age\":30" // 缺少闭合括号
  let json_parse_result = JsonSerializer::try_deserialize[Person](malformed_json)
  match json_parse_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("parse") || error.contains("invalid"))
  }
  
  // 测试XML解析错误
  let malformed_xml = "<person><name>Alice<age>30</age></person>" // 缺少闭合标签
  let xml_parse_result = XmlSerializer::try_deserialize(malformed_xml)
  match xml_parse_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("parse") || error.contains("invalid"))
  }
  
  // 测试类型不匹配错误
  let json_string = "\"not a number\""
  let type_mismatch_result = JsonSerializer::try_deserialize[Int](json_string)
  match type_mismatch_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("type") || error.contains("mismatch"))
  }
  
  // 测试Protocol Buffers版本不兼容错误
  let old_proto = ProtoMessage::new("OldPerson")
  ProtoMessage::add_field(old_proto, "name", ProtoFieldType::String, 1)
  
  let new_proto = ProtoMessage::new("NewPerson")
  ProtoMessage::add_field(new_proto, "name", ProtoFieldType::String, 1)
  ProtoMessage::add_field(new_proto, "age", ProtoFieldType::Int32, 2)
  
  let new_person = ExtendedPerson::new("Alice", 30, "alice@example.com", "555-1234")
  let new_proto_data = ProtoSerializer::serialize(new_person, new_proto)
  
  let version_mismatch_result = ProtoSerializer::try_deserialize[Person](new_proto_data, old_proto)
  // 这可能成功或失败，取决于实现
  // 如果成功，应该只解析兼容的字段
  
  // 测试序列化大小限制
  let huge_string = "A".repeat(100000000) // 100MB字符串
  let size_limit_result = JsonSerializer::try_serialize_with_limit(huge_string, 50000000) // 50MB限制
  match size_limit_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("size") || error.contains("limit"))
  }
  
  // 测试反序列化深度限制
  let deeply_nested = create_nested_json(100) // 创建100层嵌套的JSON
  let depth_limit_result = JsonSerializer::try_deserialize_with_limit(deeply_nested, 50) // 50层限制
  match depth_limit_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("depth") || error.contains("limit"))
  }
  
  // 测试安全反序列化
  let malicious_json = "{\"__proto__\":{\"admin\":true}}" // 潜在的原型污染
  let safe_result = JsonSerializer::safe_deserialize[Map[String, Any]](malicious_json)
  match safe_result {
    Ok(parsed) => {
      // 应该过滤掉危险字段
      assert_false(Map::contains_key(parsed, "__proto__"))
    }
    Err(_) => assert_true(true) // 或者直接拒绝
  }
}

// 测试9: 序列化安全性
test "序列化安全性测试" {
  // 测试敏感数据过滤
  let user_with_password = UserWithPassword::new("alice", "secret123")
  let serializer = JsonSerializer::with_security_policy()
  JsonSerializer::add_sensitive_field(serializer, "password")
  
  let filtered_json = JsonSerializer::serialize_with_filter(serializer, user_with_password)
  assert_false(filtered_json.contains("secret123"))
  assert_true(filtered_json.contains("\"name\":\"alice\""))
  
  // 测试反序列化输入验证
  let malicious_input = "{\"name\":\"<script>alert('xss')</script>\",\"age\":30}"
  let safe_parser = JsonSerializer::with_input_validation()
  JsonSerializer::add_validation_rule(safe_parser, "name", fn(value) {
    !value.contains("<script>")
  })
  
  let validation_result = JsonSerializer::try_deserialize_with_validation[Person](malicious_input, safe_parser)
  match validation_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("validation") || error.contains("unsafe"))
  }
  
  // 测试序列化数据签名
  let sensitive_data = SensitiveData::new("confidential information")
  let signing_key = "signing-key-32-bytes-long"
  
  let signed_data = SecureSerializer::serialize_and_sign(sensitive_data, signing_key)
  assert_true(signed_data.contains("data:"))
  assert_true(signed_data.contains("signature:"))
  
  let verification_result = SecureSerializer::deserialize_and_verify[SensitiveData](signed_data, signing_key)
  match verification_result {
    Ok(data) => assert_eq(SensitiveData::content(data), "confidential information")
    Err(_) => assert_true(false)
  }
  
  // 测试签名验证失败
  let wrong_key = "wrong-signing-key-32-bytes"
  let wrong_verification = SecureSerializer::try_deserialize_and_verify[SensitiveData](signed_data, wrong_key)
  match wrong_verification {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("signature") || error.contains("verify"))
  }
  
  // 测试篡改检测
  let tampered_data = signed_data.replace("confidential", "modified")
  let tampered_verification = SecureSerializer::try_deserialize_and_verify[SensitiveData](tampered_data, signing_key)
  match tampered_verification {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("signature") || error.contains("tampered"))
  }
  
  // 测试序列化数据加密
  let encrypted_data = SecureSerializer::serialize_and_encrypt(sensitive_data, "encryption-key-32")
  assert_ne(encrypted_data, "confidential information")
  
  let decrypted_result = SecureSerializer::deserialize_and_decrypt[SensitiveData](encrypted_data, "encryption-key-32")
  match decrypted_result {
    Ok(data) => assert_eq(SensitiveData::content(data), "confidential information")
    Err(_) => assert_true(false)
  }
  
  // 测试解密失败
  let wrong_encryption_key = "wrong-encryption-key-32"
  let decryption_failure = SecureSerializer::try_deserialize_and_decrypt[SensitiveData](encrypted_data, wrong_encryption_key)
  match decryption_failure {
    Ok(_) => assert_true(false) // 应该失败
    Err(error) => assert_true(error.contains("decrypt") || error.contains("key"))
  }
}

// 测试10: 跨平台序列化兼容性
test "跨平台序列化兼容性测试" {
  // 测试大端序和小端序兼容性
  let test_data = 123456789
  let little_endian = BinarySerializer::serialize_with_endian(test_data, Endianness::Little)
  let big_endian = BinarySerializer::serialize_with_endian(test_data, Endianness::Big)
  
  assert_ne(little_endian, big_endian)
  
  // 测试跨字节序解析
  let parsed_from_little = BinarySerializer::deserialize_with_endian[Int](little_endian, Endianness::Little)
  let parsed_from_big = BinarySerializer::deserialize_with_endian[Int](big_endian, Endianness::Big)
  
  assert_eq(parsed_from_little, test_data)
  assert_eq(parsed_from_big, test_data)
  
  // 测试平台特定序列化
  let platform_info = PlatformInfo::new()
  let platform_data = PlatformSerializer::serialize(platform_info)
  
  // 模拟不同平台解析
  let windows_parsed = PlatformSerializer::deserialize_for_platform(platform_data, "Windows")
  let linux_parsed = PlatformSerializer::deserialize_for_platform(platform_data, "Linux")
  let macos_parsed = PlatformSerializer::deserialize_for_platform(platform_data, "macOS")
  
  // 所有平台应该能解析基本信息
  assert_true(PlatformInfo::os_name(windows_parsed).length() > 0)
  assert_true(PlatformInfo::os_name(linux_parsed).length() > 0)
  assert_true(PlatformInfo::os_name(macos_parsed).length() > 0)
  
  // 测试版本兼容性
  let v1_data = VersionedSerializer::serialize_v1(person)
  let v2_data = VersionedSerializer::serialize_v2(person)
  
  // V2应该能解析V1数据（向后兼容）
  let v1_parsed_in_v2 = VersionedSerializer::deserialize_v2_from_v1(v1_data)
  assert_eq(Person::name(v1_parsed_in_v2), Person::name(person))
  
  // V1应该能解析V2数据（可能忽略新字段）
  let v2_parsed_in_v1 = VersionedSerializer::deserialize_v1_from_v2(v2_data)
  assert_eq(Person::name(v2_parsed_in_v1), Person::name(person))
  
  // 测试语言特定的序列化格式
  let json_with_types = JsonSerializer::serialize_with_types(person)
  assert_true(json_with_types.contains("\"__type\":\"Person\""))
  
  let cross_language_parsed = CrossLanguageSerializer::deserialize_from_any(json_with_types)
  assert_eq(CrossLanguageData::field(cross_language_parsed, "name"), "Alice")
  
  // 测试标准格式兼容性
  let standard_json = JsonSerializer::serialize(person)
  let is_compliant = JsonValidator::is_compliant_with_standard(standard_json, "RFC8259")
  assert_true(is_compliant)
  
  let standard_xml = XmlSerializer::serialize(element)
  let is_xml_compliant = XmlValidator::is_compliant_with_standard(standard_xml, "XML1.0")
  assert_true(is_xml_compliant)
}

// 辅助类型定义
type Person {
  name : String
  age : Int
  email : String
}

type PersonWithOptional {
  name : String
  age : Option[Int]
  email : Option[String]
}

type PersonWithAddress {
  name : String
  age : Int
  email : String
  address : Address
}

type Address {
  street : String
  city : String
  zip : String
}

type CustomDate {
  year : Int
  month : Int
  day : Int
}

type UserWithPassword {
  username : String
  password : String
}

type SensitiveData {
  content : String
}

type Config {
  host : String
  port : Int
  database : DatabaseConfig
  features : Array[String]
}

type DatabaseConfig {
  name : String
  user : String
  password : String
}

type PersonList {
  people : Array[Person]
}

type ExtendedPerson {
  name : String
  age : Int
  email : String
  phone : String
}

type PlatformInfo {
  os_name : String
  architecture : String
  version : String
}

type CrossLanguageData {
  type_name : String
  fields : Map[String, String]
}

// 简化的辅助函数定义（实际实现中这些应该有完整的实现）
fn Person::new(name : String, age : Int, email : String) -> Person {
  { name, age, email }
}

fn Person::name(person : Person) -> String { person.name }
fn Person::age(person : Person) -> Int { person.age }
fn Person::email(person : Person) -> String { person.email }

fn PersonWithOptional::new(name : String, age : Option[Int], email : Option[String]) -> PersonWithOptional {
  { name, age, email }
}

fn PersonWithAddress::new(name : String, age : Int, email : String, address : Address) -> PersonWithAddress {
  { name, age, email, address }
}

fn Address::new(street : String, city : String, zip : String) -> Address {
  { street, city, zip }
}

fn CustomDate::new(year : Int, month : Int, day : Int) -> CustomDate {
  { year, month, day }
}

fn CustomDate::year(date : CustomDate) -> Int { date.year }
fn CustomDate::month(date : CustomDate) -> Int { date.month }
fn CustomDate::day(date : CustomDate) -> Int { date.day }

fn UserWithPassword::new(username : String, password : String) -> UserWithPassword {
  { username, password }
}

fn SensitiveData::new(content : String) -> SensitiveData {
  { content }
}

fn SensitiveData::content(data : SensitiveData) -> String { data.content }

fn Config::new(host : String, port : Int, database : DatabaseConfig, features : Array[String]) -> Config {
  { host, port, database, features }
}

fn Config::host(config : Config) -> String { config.host }
fn Config::port(config : Config) -> Int { config.port }
fn Config::database(config : Config) -> DatabaseConfig { config.database }
fn Config::features(config : Config) -> Array[String] { config.features }

fn DatabaseConfig::new(name : String, user : String, password : String) -> DatabaseConfig {
  { name, user, password }
}

fn DatabaseConfig::name(config : DatabaseConfig) -> String { config.name }

fn PersonList::new(people : Array[Person]) -> PersonList {
  { people }
}

fn PersonList::people(list : PersonList) -> Array[Person] { list.people }

fn ExtendedPerson::new(name : String, age : Int, email : String, phone : String) -> ExtendedPerson {
  { name, age, email, phone }
}

fn PlatformInfo::new() -> PlatformInfo {
  { os_name: "", architecture: "", version: "" }
}

fn PlatformInfo::os_name(info : PlatformInfo) -> String { info.os_name }

fn CrossLanguageData::field(data : CrossLanguageData, field_name : String) -> String {
  Map::get_with_default(data.fields, field_name, "")
}

fn JsonSerializer::serialize[T](data : T) -> String {
  // JSON序列化的实现
  ""
}

fn JsonSerializer::deserialize[T](json_string : String) -> T {
  // JSON反序列化的实现
  panic("not implemented")
}

fn JsonSerializer::serialize_with_formatter[T](data : T, formatter : T -> String) -> String {
  // 带自定义格式化器的JSON序列化实现
  ""
}

fn JsonSerializer::deserialize_with_parser[T](json_string : String, parser : String -> T) -> T {
  // 带自定义解析器的JSON反序列化实现
  panic("not implemented")
}

fn JsonPath::query(json_string : String, path : String) -> String {
  // JSON路径查询的实现
  ""
}

fn JsonUtils::merge(json1 : String, json2 : String) -> String {
  // JSON合并的实现
  ""
}

fn JsonValidator::is_valid(json_string : String) -> Bool {
  // JSON验证的实现
  true
}

fn JsonFormatter::pretty_print(json_string : String, indent : Int) -> String {
  // JSON格式化的实现
  ""
}

fn XmlElement::new(name : String) -> Int {
  // 创建XML元素的实现
  0
}

fn XmlElement::new_with_namespace(name : String, namespace : String) -> Int {
  // 创建带命名空间的XML元素的实现
  0
}

fn XmlElement::new_text(name : String, text : String) -> Int {
  // 创建文本XML元素的实现
  0
}

fn XmlElement::set_attribute(element : Int, name : String, value : String) -> Unit {
  // 设置XML元素属性的实现
}

fn XmlElement::add_child(element : Int, child : Int) -> Unit {
  // 添加XML子元素的实现
}

fn XmlSerializer::serialize(element : Int) -> String {
  // XML序列化的实现
  ""
}

fn XmlSerializer::deserialize(xml_string : String) -> Int {
  // XML反序列化的实现
  0
}

fn XmlElement::name(element : Int) -> String {
  // 获取XML元素名称的实现
  ""
}

fn XmlElement::attribute(element : Int, name : String) -> Option[String] {
  // 获取XML元素属性的实现
  Some("")
}

fn XmlElement::child(element : Int, name : String) -> Int {
  // 获取XML子元素的实现
  0
}

fn XmlElement::text(element : Int) -> String {
  // 获取XML元素文本的实现
  ""
}

fn XmlXPath::query(xml_string : String, xpath : String) -> String {
  // XPath查询的实现
  ""
}

fn XmlXPath::query_all(xml_string : String, xpath : String) -> Array[String] {
  // XPath多值查询的实现
  []
}

fn XmlValidator::is_valid(xml_string : String) -> Bool {
  // XML验证的实现
  true
}

fn XmlTransformer::transform(xml_string : String, transformer : Int -> Int) -> String {
  // XML转换的实现
  ""
}

fn BinarySerializer::serialize[T](data : T) -> String {
  // 二进制序列化的实现
  ""
}

fn BinarySerializer::deserialize[T](binary_data : String) -> T {
  // 二进制反序列化的实现
  panic("not implemented")
}

fn BinarySerializer::with_version(version : Int) -> Int {
  // 创建版本化二进制序列化器的实现
  0
}

fn BinarySerializer::serialize_with_version[T](serializer : Int, data : T) -> String {
  // 版本化二进制序列化的实现
  ""
}

fn BinarySerializer::deserialize_with_version[T](serializer : Int, binary_data : String) -> T {
  // 版本化二进制反序列化的实现
  panic("not implemented")
}

fn BinarySerializer::compress(data : String) -> String {
  // 二进制数据压缩的实现
  ""
}

fn BinarySerializer::decompress(compressed_data : String) -> String {
  // 二进制数据解压缩的实现
  ""
}

fn BinarySerializer::encrypt(data : String, key : String) -> String {
  // 二进制数据加密的实现
  ""
}

fn BinarySerializer::decrypt(encrypted_data : String, key : String) -> String {
  // 二进制数据解密的实现
  ""
}

fn BinarySerializer::serialize_with_endian[T](data : T, endianness : Endianness) -> String {
  // 指定字节序的二进制序列化实现
  ""
}

fn BinarySerializer::deserialize_with_endian[T](binary_data : String, endianness : Endianness) -> T {
  // 指定字节序的二进制反序列化实现
  panic("not implemented")
}

enum Endianness {
  Little
  Big
}

fn ProtoMessage::new(name : String) -> Int {
  // 创建Protocol Buffers消息的实现
  0
}

fn ProtoMessage::add_field(message : Int, name : String, field_type : ProtoFieldType, number : Int) -> Unit {
  // 添加Protocol Buffers字段的实现
}

fn ProtoMessage::add_field(message : Int, name : String, field_type : ProtoFieldType, number : Int, sub_message : Int) -> Unit {
  // 添加Protocol Buffers消息字段的实现
}

enum ProtoFieldType {
  String
  Int32
  Int64
  Float
  Double
  Bool
  Message
  RepeatedMessage
}

fn ProtoSerializer::serialize[T](data : T, message : Int) -> String {
  // Protocol Buffers序列化的实现
  ""
}

fn ProtoSerializer::deserialize[T](proto_data : String, message : Int) -> T {
  // Protocol Buffers反序列化的实现
  panic("not implemented")
}

fn CsvSerializer::serialize[T](data : T) -> String {
  // CSV序列化的实现
  ""
}

fn CsvSerializer::deserialize[T](csv_string : String) -> T {
  // CSV反序列化的实现
  panic("not implemented")
}

fn YamlSerializer::serialize[T](data : T) -> String {
  // YAML序列化的实现
  ""
}

fn YamlSerializer::deserialize[T](yaml_string : String) -> T {
  // YAML反序列化的实现
  panic("not implemented")
}

fn TomlSerializer::serialize[T](data : T) -> String {
  // TOML序列化的实现
  ""
}

fn TomlSerializer::deserialize[T](toml_string : String) -> T {
  // TOML反序列化的实现
  panic("not implemented")
}

fn CustomSerializer::new() -> Int {
  // 创建自定义序列化器的实现
  0
}

fn CustomSerializer::register_formatter[T](serializer : Int, type_name : String, formatter : T -> String) -> Unit {
  // 注册自定义格式化器的实现
}

fn CustomSerializer::register_parser[T](serializer : Int, type_name : String, parser : String -> T) -> Unit {
  // 注册自定义解析器的实现
}

fn CustomSerializer::serialize[T](serializer : Int, data : T) -> String {
  // 自定义序列化的实现
  ""
}

fn CustomSerializer::deserialize[T](serializer : Int, data : String) -> T {
  // 自定义反序列化的实现
  panic("not implemented")
}

fn JsonSerializer::try_deserialize[T](json_string : String) -> Result[T, String] {
  // 尝试JSON反序列化的实现
  Err("not implemented")
}

fn XmlSerializer::try_deserialize(xml_string : String) -> Result[Int, String] {
  // 尝试XML反序列化的实现
  Err("not implemented")
}

fn JsonSerializer::try_serialize_with_limit[T](data : T, size_limit : Int) -> Result[String, String] {
  // 带大小限制的JSON序列化实现
  Err("not implemented")
}

fn JsonSerializer::try_deserialize_with_limit[T](json_string : String, depth_limit : Int) -> Result[T, String] {
  // 带深度限制的JSON反序列化实现
  Err("not implemented")
}

fn JsonSerializer::safe_deserialize[T](json_string : String) -> Result[T, String] {
  // 安全JSON反序列化的实现
  Err("not implemented")
}

fn JsonSerializer::with_security_policy() -> Int {
  // 创建安全JSON序列化器的实现
  0
}

fn JsonSerializer::add_sensitive_field(serializer : Int, field_name : String) -> Unit {
  // 添加敏感字段的实现
}

fn JsonSerializer::serialize_with_filter[T](serializer : Int, data : T) -> String {
  // 带过滤器的JSON序列化实现
  ""
}

fn JsonSerializer::with_input_validation() -> Int {
  // 创建带输入验证的JSON反序列化器的实现
  0
}

fn JsonSerializer::add_validation_rule[T](serializer : Int, field_name : String, validator : T -> Bool) -> Unit {
  // 添加验证规则的实现
}

fn JsonSerializer::try_deserialize_with_validation[T](json_string : String, serializer : Int) -> Result[T, String] {
  // 带验证的JSON反序列化实现
  Err("not implemented")
}

fn SecureSerializer::serialize_and_sign[T](data : T, key : String) -> String {
  // 序列化并签名的实现
  ""
}

fn SecureSerializer::deserialize_and_verify[T](signed_data : String, key : String) -> Result[T, String] {
  // 反序列化并验证的实现
  Err("not implemented")
}

fn SecureSerializer::try_deserialize_and_verify[T](signed_data : String, key : String) -> Result[T, String] {
  // 尝试反序列化并验证的实现
  Err("not implemented")
}

fn SecureSerializer::serialize_and_encrypt[T](data : T, key : String) -> String {
  // 序列化并加密的实现
  ""
}

fn SecureSerializer::deserialize_and_decrypt[T](encrypted_data : String, key : String) -> Result[T, String] {
  // 反序列化并解密的实现
  Err("not implemented")
}

fn SecureSerializer::try_deserialize_and_decrypt[T](encrypted_data : String, key : String) -> Result[T, String] {
  // 尝试反序列化并解密的实现
  Err("not implemented")
}

fn PlatformSerializer::serialize(platform_info : PlatformInfo) -> String {
  // 平台特定序列化的实现
  ""
}

fn PlatformSerializer::deserialize_for_platform(data : String, platform : String) -> PlatformInfo {
  // 平台特定反序列化的实现
  PlatformInfo::new()
}

fn VersionedSerializer::serialize_v1[T](data : T) -> String {
  // V1版本序列化的实现
  ""
}

fn VersionedSerializer::serialize_v2[T](data : T) -> String {
  // V2版本序列化的实现
  ""
}

fn VersionedSerializer::deserialize_v2_from_v1(data : String) -> Person {
  // V2解析V1数据的实现
  Person::new("", 0, "")
}

fn VersionedSerializer::deserialize_v1_from_v2(data : String) -> Person {
  // V1解析V2数据的实现
  Person::new("", 0, "")
}

fn JsonSerializer::serialize_with_types[T](data : T) -> String {
  // 带类型信息的JSON序列化实现
  ""
}

fn CrossLanguageSerializer::deserialize_from_any(data : String) -> CrossLanguageData {
  // 跨语言反序列化的实现
  { type_name: "", fields: Map::empty() }
}

fn JsonValidator::is_compliant_with_standard(json_string : String, standard : String) -> Bool {
  // 标准兼容性检查的实现
  true
}

fn XmlValidator::is_compliant_with_standard(xml_string : String, standard : String) -> Bool {
  // XML标准兼容性检查的实现
  true
}

fn Time::now() -> Time {
  // 获取当前时间的实现
  { hour: 0, minute: 0, second: 0 }
}

fn Time::elapsed_since(start_time : Time) -> Int {
  // 计算经过时间的实现
  0
}

fn create_nested_json(depth : Int) -> String {
  // 创建嵌套JSON的实现
  if depth <= 0 {
    "\"leaf\""
  } else {
    "{\"nested\":" + create_nested_json(depth - 1) + "}"
  }
}