// Azimuth Data Serialization and Deserialization Test Suite
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: JSON-like Serialization
test "json-like serialization" {
  // Define data structures for serialization
  type Person {
    name : String
    age : Int
    email : String
  }
  
  type Address {
    street : String
    city : String
    country : String
    postal_code : String
  }
  
  type User {
    id : Int
    person : Person
    address : Address
    is_active : Bool
    tags : Array[String]
  }
  
  // Serialization functions
  let serialize_person = fn(person) {
    "{\"name\":\"" + person.name + "\",\"age\":" + person.age.to_string() + ",\"email\":\"" + person.email + "\"}"
  }
  
  let serialize_address = fn(address) {
    "{\"street\":\"" + address.street + "\",\"city\":\"" + address.city + "\",\"country\":\"" + address.country + "\",\"postal_code\":\"" + address.postal_code + "\"}"
  }
  
  let serialize_user = fn(user) {
    let person_json = serialize_person(user.person)
    let address_json = serialize_address(user.address)
    
    let tags_json = "["
    let mut first_tag = true
    for tag in user.tags {
      if not first_tag {
        tags_json = tags_json + ","
      }
      tags_json = tags_json + "\"" + tag + "\""
      first_tag = false
    }
    tags_json = tags_json + "]"
    
    "{\"id\":" + user.id.to_string() + ",\"person\":" + person_json + ",\"address\":" + address_json + ",\"is_active\":" + (if user.is_active { "true" } else { "false" }) + ",\"tags\":" + tags_json + "}"
  }
  
  // Create test data
  let person = {
    name: "John Doe",
    age: 30,
    email: "john@example.com"
  }
  
  let address = {
    street: "123 Main St",
    city: "New York",
    country: "USA",
    postal_code: "10001"
  }
  
  let user = {
    id: 12345,
    person: person,
    address: address,
    is_active: true,
    tags: ["developer", "javascript", "react"]
  }
  
  // Test serialization
  let person_json = serialize_person(person)
  let address_json = serialize_address(address)
  let user_json = serialize_user(user)
  
  // Verify JSON structure
  assert_true(person_json.contains("\"name\":\"John Doe\""))
  assert_true(person_json.contains("\"age\":30"))
  assert_true(person_json.contains("\"email\":\"john@example.com\""))
  
  assert_true(address_json.contains("\"street\":\"123 Main St\""))
  assert_true(address_json.contains("\"city\":\"New York\""))
  assert_true(address_json.contains("\"country\":\"USA\""))
  assert_true(address_json.contains("\"postal_code\":\"10001\""))
  
  assert_true(user_json.contains("\"id\":12345"))
  assert_true(user_json.contains("\"is_active\":true"))
  assert_true(user_json.contains("\"tags\":[\"developer\",\"javascript\",\"react\"]"))
}

// Test 2: CSV Serialization
test "csv serialization" {
  // Define data structure for CSV
  type SalesRecord {
    id : Int
    product : String
    quantity : Int
    price : Float
    date : String
    region : String
  }
  
  // CSV serialization function
  let serialize_sales_record = fn(record) {
    record.id.to_string() + "," + 
    record.product + "," + 
    record.quantity.to_string() + "," + 
    record.price.to_string() + "," + 
    record.date + "," + 
    record.region
  }
  
  let serialize_sales_records = fn(records) {
    let mut csv = "ID,Product,Quantity,Price,Date,Region\n"
    
    for record in records {
      csv = csv + serialize_sales_record(record) + "\n"
    }
    
    csv
  }
  
  // Create test data
  let records = [
    {
      id: 1,
      product: "Laptop",
      quantity: 5,
      price: 999.99,
      date: "2023-01-15",
      region: "North"
    },
    {
      id: 2,
      product: "Mouse",
      quantity: 20,
      price: 25.50,
      date: "2023-01-16",
      region: "South"
    },
    {
      id: 3,
      product: "Keyboard",
      quantity: 10,
      price: 75.00,
      date: "2023-01-17",
      region: "East"
    }
  ]
  
  // Test serialization
  let csv = serialize_sales_records(records)
  
  // Verify CSV structure
  assert_true(csv.contains("ID,Product,Quantity,Price,Date,Region"))
  assert_true(csv.contains("1,Laptop,5,999.99,2023-01-15,North"))
  assert_true(csv.contains("2,Mouse,20,25.5,2023-01-16,South"))
  assert_true(csv.contains("3,Keyboard,10,75.0,2023-01-17,East"))
  
  // Test CSV parsing simulation
  let parse_csv_line = fn(line) {
    let mut fields = []
    let mut current_field = ""
    let mut in_quotes = false
    
    for i in 0..line.length() {
      let char = line[i]
      
      if char == '\"' {
        in_quotes = not in_quotes
      } else if char == ',' and not in_quotes {
        fields = fields @ [current_field]
        current_field = ""
      } else {
        current_field = current_field + char.to_string()
      }
    }
    
    fields = fields @ [current_field]
    fields
  }
  
  let header_fields = parse_csv_line("ID,Product,Quantity,Price,Date,Region")
  assert_eq(header_fields.length(), 6)
  assert_eq(header_fields[0], "ID")
  assert_eq(header_fields[1], "Product")
  
  let record_fields = parse_csv_line("1,Laptop,5,999.99,2023-01-15,North")
  assert_eq(record_fields.length(), 6)
  assert_eq(record_fields[0], "1")
  assert_eq(record_fields[1], "Laptop")
}

// Test 3: Binary Serialization Simulation
test "binary serialization simulation" {
  // Simulate binary serialization using byte arrays (represented as integers)
  
  let int_to_bytes = fn(value) {
    [
      (value >> 24) & 0xFF,
      (value >> 16) & 0xFF,
      (value >> 8) & 0xFF,
      value & 0xFF
    ]
  }
  
  let float_to_bytes = fn(value) {
    // Simplified float to bytes conversion
    let int_value = value.to_int()
    [
      (int_value >> 24) & 0xFF,
      (int_value >> 16) & 0xFF,
      (int_value >> 8) & 0xFF,
      int_value & 0xFF
    ]
  }
  
  let string_to_bytes = fn(str) {
    let mut bytes = [str.length() & 0xFF, (str.length() >> 8) & 0xFF]
    for i in 0..str.length() {
      bytes = bytes @ [str[i].to_int()]
    }
    bytes
  }
  
  let serialize_telemetry_data = fn(timestamp, sensor_id, value, unit) {
    let mut bytes = []
    
    // Serialize timestamp (4 bytes)
    bytes = bytes @ int_to_bytes(timestamp)
    
    // Serialize sensor_id (4 bytes)
    bytes = bytes @ int_to_bytes(sensor_id)
    
    // Serialize value (4 bytes)
    bytes = bytes @ float_to_bytes(value)
    
    // Serialize unit (variable length)
    bytes = bytes @ string_to_bytes(unit)
    
    bytes
  }
  
  // Test serialization
  let timestamp = 1640995200  // 2022-01-01 00:00:00
  let sensor_id = 42
  let value = 23.5
  let unit = "Celsius"
  
  let bytes = serialize_telemetry_data(timestamp, sensor_id, value, unit)
  
  // Verify byte array structure
  assert_eq(bytes.length(), 4 + 4 + 4 + 2 + 7)  // timestamp + sensor_id + value + unit_length + unit_chars
  
  // Verify timestamp bytes
  assert_eq(bytes[0], (timestamp >> 24) & 0xFF)
  assert_eq(bytes[1], (timestamp >> 16) & 0xFF)
  assert_eq(bytes[2], (timestamp >> 8) & 0xFF)
  assert_eq(bytes[3], timestamp & 0xFF)
  
  // Verify sensor_id bytes
  assert_eq(bytes[4], (sensor_id >> 24) & 0xFF)
  assert_eq(bytes[5], (sensor_id >> 16) & 0xFF)
  assert_eq(bytes[6], (sensor_id >> 8) & 0xFF)
  assert_eq(bytes[7], sensor_id & 0xFF)
  
  // Verify unit length bytes
  assert_eq(bytes[16], 7)  // "Celsius" has 7 characters
  assert_eq(bytes[17], 0)
  
  // Test deserialization simulation
  let bytes_to_int = fn(bytes, offset) {
    (bytes[offset] << 24) | (bytes[offset + 1] << 16) | (bytes[offset + 2] << 8) | bytes[offset + 3]
  }
  
  let bytes_to_string = fn(bytes, offset) {
    let length = bytes[offset] | (bytes[offset + 1] << 8)
    let mut result = ""
    for i in 0..length {
      result = result + bytes[offset + 2 + i].to_char().to_string()
    }
    result
  }
  
  let deserialized_timestamp = bytes_to_int(bytes, 0)
  let deserialized_sensor_id = bytes_to_int(bytes, 4)
  let deserialized_unit = bytes_to_string(bytes, 16)
  
  assert_eq(deserialized_timestamp, timestamp)
  assert_eq(deserialized_sensor_id, sensor_id)
  assert_eq(deserialized_unit, "Celsius")
}

// Test 4: XML-like Serialization
test "xml-like serialization" {
  // Define data structure for XML
  type Product {
    id : Int
    name : String
    price : Float
    category : String
    in_stock : Bool
    description : String
  }
  
  // XML serialization function
  let escape_xml = fn(text) {
    let mut result = ""
    for i in 0..text.length() {
      let char = text[i]
      match char {
        '<' => result = result + "&lt;"
        '>' => result = result + "&gt;"
        '&' => result = result + "&amp;"
        '\"' => result = result + "&quot;"
        '\'' => result = result + "&apos;"
        _ => result = result + char.to_string()
      }
    }
    result
  }
  
  let serialize_product = fn(product) {
    let escaped_name = escape_xml(product.name)
    let escaped_description = escape_xml(product.description)
    let escaped_category = escape_xml(product.category)
    
    "<product id=\"" + product.id.to_string() + "\">\n" +
    "  <name>" + escaped_name + "</name>\n" +
    "  <price>" + product.price.to_string() + "</price>\n" +
    "  <category>" + escaped_category + "</category>\n" +
    "  <in_stock>" + (if product.in_stock { "true" } else { "false" }) + "</in_stock>\n" +
    "  <description>" + escaped_description + "</description>\n" +
    "</product>"
  }
  
  let serialize_products = fn(products) {
    let mut xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<products>\n"
    
    for product in products {
      xml = xml + serialize_product(product) + "\n"
    }
    
    xml = xml + "</products>"
    xml
  }
  
  // Create test data
  let products = [
    {
      id: 101,
      name: "Smartphone",
      price: 699.99,
      category: "Electronics",
      in_stock: true,
      description: "Latest model with advanced features"
    },
    {
      id: 102,
      name: "Laptop <Pro>",
      price: 1299.99,
      category: "Computers",
      in_stock: false,
      description: "High-performance laptop for professionals"
    }
  ]
  
  // Test serialization
  let xml = serialize_products(products)
  
  // Verify XML structure
  assert_true(xml.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
  assert_true(xml.contains("<products>"))
  assert_true(xml.contains("</products>"))
  assert_true(xml.contains("<product id=\"101\">"))
  assert_true(xml.contains("<name>Smartphone</name>"))
  assert_true(xml.contains("<price>699.99</price>"))
  assert_true(xml.contains("<category>Electronics</category>"))
  assert_true(xml.contains("<in_stock>true</in_stock>"))
  assert_true(xml.contains("<description>Latest model with advanced features</description>"))
  
  // Test XML escaping
  assert_true(xml.contains("<name>Laptop &lt;Pro&gt;</name>"))
}

// Test 5: Protocol Buffers-like Serialization
test "protocol buffers-like serialization" {
  // Simulate Protocol Buffers serialization with field numbers and wire types
  
  let write_varint = fn(value, bytes) {
    let mut result = bytes
    let mut v = value
    
    while v >= 128 {
      result = result @ [(v & 0x7F) | 0x80]
      v = v >> 7
    }
    
    result = result @ [v]
    result
  }
  
  let serialize_field = fn(field_number, wire_type, value) {
    let mut bytes = []
    let key = (field_number << 3) | wire_type
    bytes = write_varint(key, bytes)
    
    match wire_type {
      0 => {  // Varint
        bytes = write_varint(value, bytes)
      }
      1 => {  // 64-bit
        // Simplified: just add 8 bytes
        for i in 0..8 {
          bytes = bytes @ [0]
        }
      }
      2 => {  // Length-delimited
        let str_value = value
        bytes = write_varint(str_value.length(), bytes)
        for i in 0..str_value.length() {
          bytes = bytes @ [str_value[i].to_int()]
        }
      }
      5 => {  // 32-bit
        // Simplified: just add 4 bytes
        for i in 0..4 {
          bytes = bytes @ [0]
        }
      }
      _ => ()
    }
    
    bytes
  }
  
  // Simulate a message with different field types
  let serialize_person_message = fn(id, name, email, age) {
    let mut bytes = []
    
    // Field 1: id (int32, varint)
    bytes = serialize_field(1, 0, id)
    
    // Field 2: name (string, length-delimited)
    bytes = serialize_field(2, 2, name)
    
    // Field 3: email (string, length-delimited)
    bytes = serialize_field(3, 2, email)
    
    // Field 4: age (int32, varint)
    bytes = serialize_field(4, 0, age)
    
    bytes
  }
  
  // Test serialization
  let id = 12345
  let name = "John Doe"
  let email = "john@example.com"
  let age = 30
  
  let bytes = serialize_person_message(id, name, email, age)
  
  // Verify byte array is not empty
  assert_true(bytes.length() > 0)
  
  // Verify structure by checking field markers
  // Field 1 (id): key = (1 << 3) | 0 = 8
  assert_eq(bytes[0], 8)
  
  // Find and verify name field
  let mut found_name_field = false
  for i in 0..bytes.length() - 1 {
    if bytes[i] == 18 {  // Field 2 key: (2 << 3) | 2 = 18
      found_name_field = true
      break
    }
  }
  assert_true(found_name_field)
}

// Test 6: Custom Binary Format
test "custom binary format" {
  // Define a custom binary format for telemetry data
  
  type TelemetryPacket {
    header : Array[Int]
    payload : Array[Float]
    checksum : Int
  }
  
  let calculate_checksum = fn(data) {
    let mut sum = 0
    for byte in data {
      sum = sum + byte
    }
    sum & 0xFF  // Keep only the lowest 8 bits
  }
  
  let serialize_telemetry_packet = fn(packet_type, sequence_number, timestamp, sensor_data) {
    let mut bytes = []
    
    // Header (8 bytes)
    // Packet type (1 byte)
    bytes = bytes @ [packet_type & 0xFF]
    
    // Sequence number (4 bytes)
    bytes = bytes @ [
      (sequence_number >> 24) & 0xFF,
      (sequence_number >> 16) & 0xFF,
      (sequence_number >> 8) & 0xFF,
      sequence_number & 0xFF
    ]
    
    // Timestamp (4 bytes)
    bytes = bytes @ [
      (timestamp >> 24) & 0xFF,
      (timestamp >> 16) & 0xFF,
      (timestamp >> 8) & 0xFF,
      timestamp & 0xFF
    ]
    
    // Payload length (2 bytes)
    let payload_length = sensor_data.length()
    bytes = bytes @ [
      (payload_length >> 8) & 0xFF,
      payload_length & 0xFF
    ]
    
    // Payload (4 bytes per float)
    for value in sensor_data {
      let int_value = value.to_int()
      bytes = bytes @ [
        (int_value >> 24) & 0xFF,
        (int_value >> 16) & 0xFF,
        (int_value >> 8) & 0xFF,
        int_value & 0xFF
      ]
    }
    
    // Checksum (1 byte)
    let checksum = calculate_checksum(bytes)
    bytes = bytes @ [checksum]
    
    bytes
  }
  
  // Test serialization
  let packet_type = 1
  let sequence_number = 12345
  let timestamp = 1640995200
  let sensor_data = [23.5, 24.0, 23.8, 24.2]
  
  let bytes = serialize_telemetry_packet(packet_type, sequence_number, timestamp, sensor_data)
  
  // Verify header
  assert_eq(bytes[0], packet_type)
  assert_eq(bytes[1], (sequence_number >> 24) & 0xFF)
  assert_eq(bytes[5], timestamp & 0xFF)
  
  // Verify payload length
  assert_eq(bytes[9], 0)
  assert_eq(bytes[10], sensor_data.length())
  
  // Verify payload
  let payload_start = 11
  for i in 0..sensor_data.length() {
    let value_start = payload_start + i * 4
    let int_value = sensor_data[i].to_int()
    assert_eq(bytes[value_start], (int_value >> 24) & 0xFF)
  }
  
  // Verify checksum
  let expected_checksum = calculate_checksum(bytes[0:bytes.length() - 1])
  assert_eq(bytes[bytes.length() - 1], expected_checksum)
}

// Test 7: MessagePack-like Serialization
test "messagepack-like serialization" {
  // Simulate MessagePack serialization
  
  let serialize_str = fn(str) {
    let mut bytes = []
    let length = str.length()
    
    if length < 32 {
      // FixStr
      bytes = bytes @ [0xA0 | length]
    } else if length < 256 {
      // Str 8
      bytes = bytes @ [0xD9, length]
    } else {
      // Simplified: just use Str 8 for this test
      bytes = bytes @ [0xD9, length]
    }
    
    for i in 0..length {
      bytes = bytes @ [str[i].to_int()]
    }
    
    bytes
  }
  
  let serialize_int = fn(value) {
    let mut bytes = []
    
    if value >= 0 {
      if value < 128 {
        // Positive FixInt
        bytes = bytes @ [value]
      } else if value < 256 {
        // Uint 8
        bytes = bytes @ [0xCC, value]
      } else if value < 65536 {
        // Uint 16
        bytes = bytes @ [0xCD, (value >> 8) & 0xFF, value & 0xFF]
      } else {
        // Simplified: just use Uint 16 for this test
        bytes = bytes @ [0xCD, (value >> 8) & 0xFF, value & 0xFF]
      }
    } else {
      // Simplified: just handle positive values for this test
      bytes = bytes @ [0]
    }
    
    bytes
  }
  
  let serialize_array = fn(array) {
    let mut bytes = []
    let length = array.length()
    
    if length < 16 {
      // FixArray
      bytes = bytes @ [0x90 | length]
    } else {
      // Simplified: just use FixArray for this test
      bytes = bytes @ [0x90 | (length & 0x0F)]
    }
    
    for element in array {
      bytes = bytes @ serialize_int(element)
    }
    
    bytes
  }
  
  let serialize_map = fn(map) {
    let mut bytes = []
    let length = map.length()
    
    if length < 16 {
      // FixMap
      bytes = bytes @ [0x80 | length]
    } else {
      // Simplified: just use FixMap for this test
      bytes = bytes @ [0x80 | (length & 0x0F)]
    }
    
    for (key, value) in map {
      bytes = bytes @ serialize_str(key)
      bytes = bytes @ serialize_int(value)
    }
    
    bytes
  }
  
  // Test serialization
  let name = "Alice"
  let age = 30
  let scores = [85, 90, 78, 92]
  let attributes = [("height", 165), ("weight", 60)]
  
  let name_bytes = serialize_str(name)
  let age_bytes = serialize_int(age)
  let scores_bytes = serialize_array(scores)
  let attributes_bytes = serialize_map(attributes)
  
  // Verify string serialization
  assert_eq(name_bytes[0], 0xA5 | name.length())  // FixStr format byte
  assert_eq(name_bytes[1], 'A'.to_int())
  assert_eq(name_bytes[name_bytes.length() - 1], 'e'.to_int())
  
  // Verify integer serialization
  assert_eq(age_bytes[0], age)  // Positive FixInt
  
  // Verify array serialization
  assert_eq(scores_bytes[0], 0x90 | scores.length())  // FixArray format byte
  
  // Verify map serialization
  assert_eq(attributes_bytes[0], 0x80 | attributes.length())  // FixMap format byte
}

// Test 8: YAML-like Serialization
test "yaml-like serialization" {
  // Simulate YAML serialization
  
  type Config {
    database : DatabaseConfig
    server : ServerConfig
    logging : LoggingConfig
  }
  
  type DatabaseConfig {
    host : String
    port : Int
    username : String
    password : String
    name : String
  }
  
  type ServerConfig {
    host : String
    port : Int
    ssl_enabled : Bool
    max_connections : Int
  }
  
  type LoggingConfig {
    level : String
    file : String
    max_size : String
    backup_count : Int
  }
  
  let indent = fn(level) {
    let mut result = ""
    for i in 0..level {
      result = result + "  "
    }
    result
  }
  
  let serialize_database_config = fn(config, level) {
    let ind = indent(level)
    ind + "host: " + config.host + "\n" +
    ind + "port: " + config.port.to_string() + "\n" +
    ind + "username: " + config.username + "\n" +
    ind + "password: " + config.password + "\n" +
    ind + "name: " + config.name
  }
  
  let serialize_server_config = fn(config, level) {
    let ind = indent(level)
    ind + "host: " + config.host + "\n" +
    ind + "port: " + config.port.to_string() + "\n" +
    ind + "ssl_enabled: " + (if config.ssl_enabled { "true" } else { "false" }) + "\n" +
    ind + "max_connections: " + config.max_connections.to_string()
  }
  
  let serialize_logging_config = fn(config, level) {
    let ind = indent(level)
    ind + "level: " + config.level + "\n" +
    ind + "file: " + config.file + "\n" +
    ind + "max_size: " + config.max_size + "\n" +
    ind + "backup_count: " + config.backup_count.to_string()
  }
  
  let serialize_config = fn(config) {
    "database:\n" +
    serialize_database_config(config.database, 1) + "\n" +
    "server:\n" +
    serialize_server_config(config.server, 1) + "\n" +
    "logging:\n" +
    serialize_logging_config(config.logging, 1)
  }
  
  // Create test data
  let config = {
    database: {
      host: "localhost",
      port: 5432,
      username: "admin",
      password: "secret123",
      name: "azimuth_db"
    },
    server: {
      host: "0.0.0.0",
      port: 8080,
      ssl_enabled: true,
      max_connections: 100
    },
    logging: {
      level: "info",
      file: "/var/log/azimuth.log",
      max_size: "10MB",
      backup_count: 5
    }
  }
  
  // Test serialization
  let yaml = serialize_config(config)
  
  // Verify YAML structure
  assert_true(yaml.contains("database:"))
  assert_true(yaml.contains("server:"))
  assert_true(yaml.contains("logging:"))
  assert_true(yaml.contains("host: localhost"))
  assert_true(yaml.contains("port: 5432"))
  assert_true(yaml.contains("ssl_enabled: true"))
  assert_true(yaml.contains("level: info"))
  assert_true(yaml.contains("max_size: 10MB"))
  
  // Verify indentation
  assert_true(yaml.contains("  host: localhost"))
  assert_true(yaml.contains("    username: admin"))
}

// Test 9: URL-encoded Serialization
test "url-encoded serialization" {
  // Simulate URL-encoded form data serialization
  
  let url_encode = fn(text) {
    let mut result = ""
    for i in 0..text.length() {
      let char = text[i]
      match char {
        ' ' => result = result + "+"
        'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '~' => result = result + char.to_string()
        _ => {
          // URL encode as %XX
          let hex_chars = "0123456789ABCDEF"
          let char_code = char.to_int()
          result = result + "%" + hex_chars[char_code >> 4].to_string() + hex_chars[char_code & 0x0F].to_string()
        }
      }
    }
    result
  }
  
  let serialize_form_data = fn(data) {
    let mut result = ""
    let mut first = true
    
    for (key, value) in data {
      if not first {
        result = result + "&"
      }
      result = result + url_encode(key) + "=" + url_encode(value)
      first = false
    }
    
    result
  }
  
  // Test serialization
  let form_data = [
    ("name", "John Doe"),
    ("email", "john@example.com"),
    ("age", "30"),
    ("city", "New York"),
    ("message", "Hello, world!")
  ]
  
  let encoded = serialize_form_data(form_data)
  
  // Verify URL encoding
  assert_true(encoded.contains("name=John+Doe"))
  assert_true(encoded.contains("email=john%40example.com"))  // @ becomes %40
  assert_true(encoded.contains("age=30"))
  assert_true(encoded.contains("city=New+York"))
  assert_true(encoded.contains("message=Hello%2C+world%21"))  // , becomes %2C and ! becomes %21
  
  // Verify field separators
  assert_true(encoded.contains("&"))
  
  // Test decoding simulation
  let url_decode = fn(encoded_text) {
    let mut result = ""
    let i = 0
    
    while i < encoded_text.length() {
      let char = encoded_text[i]
      match char {
        '+' => result = result + " "
        '%' => {
          if i + 2 < encoded_text.length() {
            let hex1 = encoded_text[i + 1]
            let hex2 = encoded_text[i + 2]
            
            let hex_to_int = fn(hex_char) {
              match hex_char {
                '0'..'9' => hex_char.to_int() - '0'.to_int()
                'A'..'F' => hex_char.to_int() - 'A'.to_int() + 10
                'a'..'f' => hex_char.to_int() - 'a'.to_int() + 10
                _ => 0
              }
            }
            
            let char_code = (hex_to_int(hex1) << 4) | hex_to_int(hex2)
            result = result + char_code.to_char().to_string()
            i = i + 2
          }
        }
        _ => result = result + char.to_string()
      }
      i = i + 1
    }
    
    result
  }
  
  let decoded_name = url_decode("John+Doe")
  let decoded_email = url_decode("john%40example.com")
  let decoded_message = url_decode("Hello%2C+world%21")
  
  assert_eq(decoded_name, "John Doe")
  assert_eq(decoded_email, "john@example.com")
  assert_eq(decoded_message, "Hello, world!")
}

// Test 10: Base64 Serialization
test "base64 serialization" {
  // Simulate Base64 encoding and decoding
  
  let base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  
  let encode_base64 = fn(data) {
    let mut result = ""
    let i = 0
    
    while i < data.length() {
      let b1 = if i < data.length() { data[i] } else { 0 }
      let b2 = if i + 1 < data.length() { data[i + 1] } else { 0 }
      let b3 = if i + 2 < data.length() { data[i + 2] } else { 0 }
      
      let combined = (b1 << 16) | (b2 << 8) | b3
      
      let index1 = (combined >> 18) & 0x3F
      let index2 = (combined >> 12) & 0x3F
      let index3 = (combined >> 6) & 0x3F
      let index4 = combined & 0x3F
      
      result = result + base64_chars[index1].to_string()
      result = result + base64_chars[index2].to_string()
      
      if i + 1 < data.length() {
        result = result + base64_chars[index3].to_string()
      } else {
        result = result + "="
      }
      
      if i + 2 < data.length() {
        result = result + base64_chars[index4].to_string()
      } else {
        result = result + "="
      }
      
      i = i + 3
    }
    
    result
  }
  
  let decode_base64 = fn(encoded) {
    let mut result = []
    let i = 0
    
    while i < encoded.length() {
      if i + 3 < encoded.length() {
        let char_to_value = fn(c) {
          if c >= 'A' and c <= 'Z' {
            c.to_int() - 'A'.to_int()
          } else if c >= 'a' and c <= 'z' {
            c.to_int() - 'a'.to_int() + 26
          } else if c >= '0' and c <= '9' {
            c.to_int() - '0'.to_int() + 52
          } else if c == '+' {
            62
          } else if c == '/' {
            63
          } else {
            0  // For '=' padding
          }
        }
        
        let c1 = char_to_value(encoded[i])
        let c2 = char_to_value(encoded[i + 1])
        let c3 = char_to_value(encoded[i + 2])
        let c4 = char_to_value(encoded[i + 3])
        
        let combined = (c1 << 18) | (c2 << 12) | (c3 << 6) | c4
        
        let b1 = (combined >> 16) & 0xFF
        let b2 = (combined >> 8) & 0xFF
        let b3 = combined & 0xFF
        
        result = result @ [b1]
        
        if encoded[i + 2] != '=' {
          result = result @ [b2]
        }
        
        if encoded[i + 3] != '=' {
          result = result @ [b3]
        }
      }
      
      i = i + 4
    }
    
    result
  }
  
  // Test encoding and decoding
  let original_data = []
  for i in 0..26 {
    original_data = original_data @ ['A'.to_int() + i]
  }
  
  let encoded = encode_base64(original_data)
  let decoded = decode_base64(encoded)
  
  // Verify encoding
  assert_eq(encoded, "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo=")
  
  // Verify decoding
  assert_eq(decoded.length(), original_data.length())
  for i in 0..original_data.length() {
    assert_eq(decoded[i], original_data[i])
  }
  
  // Test with shorter data (not divisible by 3)
  let short_data = ['M'.to_int(), 'a'.to_int(), 'n'.to_int()]
  let short_encoded = encode_base64(short_data)
  let short_decoded = decode_base64(short_encoded)
  
  assert_eq(short_encoded, "TWFu")
  assert_eq(short_decoded.length(), 3)
  assert_eq(short_decoded[0], 'M'.to_int())
  assert_eq(short_decoded[1], 'a'.to_int())
  assert_eq(short_decoded[2], 'n'.to_int())
}