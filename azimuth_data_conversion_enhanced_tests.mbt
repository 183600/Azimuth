// Azimuth Enhanced Data Conversion Tests
// This file contains enhanced test cases for data conversion operations

// Test 1: Numeric Type Conversion
test "numeric type conversion" {
  // Integer to Float conversion
  let int_val = 42
  let float_val = int_val.to_float()
  assert_eq(float_val, 42.0)
  
  // Float to Integer conversion
  let float_val2 = 3.14
  let int_val2 = float_val2.to_int()
  assert_eq(int_val2, 3)
  
  // String to Integer conversion
  let str_int = "123"
  let parsed_int = str_int.parse_int()
  match parsed_int {
    Some(value) => assert_eq(value, 123)
    None => assert_true(false)
  }
  
  // String to Float conversion
  let str_float = "3.14159"
  let parsed_float = str_float.parse_float()
  match parsed_float {
    Some(value) => assert_eq(value, 3.14159)
    None => assert_true(false)
  }
  
  // Integer to String conversion
  let int_to_str = 456.to_string()
  assert_eq(int_to_str, "456")
  
  // Float to String conversion
  let float_to_str = 7.89.to_string()
  assert_eq(float_to_str, "7.89")
}

// Test 2: Boolean Conversion Operations
test "boolean conversion operations" {
  // String to Boolean conversion
  let str_true = "true"
  let str_false = "false"
  
  let bool_from_true = str_true.parse_bool()
  match bool_from_true {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let bool_from_false = str_false.parse_bool()
  match bool_from_false {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  // Integer to Boolean conversion (0 = false, non-zero = true)
  let zero_int = 0
  let non_zero_int = 5
  
  let bool_from_zero = zero_int.to_bool()
  assert_false(bool_from_zero)
  
  let bool_from_non_zero = non_zero_int.to_bool()
  assert_true(bool_from_non_zero)
  
  // Boolean to String conversion
  let bool_str_true = true.to_string()
  let bool_str_false = false.to_string()
  
  assert_eq(bool_str_true, "true")
  assert_eq(bool_str_false, "false")
}

// Test 3: Array Conversion Operations
test "array conversion operations" {
  // List to Array conversion
  let list_vals = [1, 2, 3, 4, 5]
  let array_vals = list_vals.to_array()
  assert_eq(array_vals.length(), 5)
  assert_eq(array_vals[0], 1)
  assert_eq(array_vals[4], 5)
  
  // Array to List conversion
  let array_vals2 = [10, 20, 30]
  let list_vals2 = array_vals2.to_list()
  assert_eq(list_vals2.length(), 3)
  assert_eq(list_vals2[0], 10)
  assert_eq(list_vals2[2], 30)
  
  // String to Array conversion (split by character)
  let str_to_split = "hello"
  let char_array = str_to_split.to_char_array()
  assert_eq(char_array.length(), 5)
  assert_eq(char_array[0], 'h')
  assert_eq(char_array[4], 'o')
  
  // Array to String conversion (join)
  let array_to_join = ['a', 'b', 'c']
  let joined_str = array_to_join.join("")
  assert_eq(joined_str, "abc")
}

// Test 4: Data Format Conversion
test "data format conversion" {
  // JSON String to Object conversion
  let json_str = "{\"name\":\"test\",\"value\":42}"
  let parsed_obj = json_str.parse_json()
  match parsed_obj {
    Some(obj) => {
      match obj.get("name") {
        Some(name) => assert_eq(name, "test")
        None => assert_true(false)
      }
      match obj.get("value") {
        Some(value) => assert_eq(value, 42)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Object to JSON String conversion
  let obj_to_convert = {
    "id": 123,
    "status": "active",
    "score": 95.5
  }
  let json_output = obj_to_convert.to_json()
  assert_true(json_output.contains("\"id\":123"))
  assert_true(json_output.contains("\"status\":\"active\""))
  assert_true(json_output.contains("\"score\":95.5"))
  
  // Base64 Encoding/Decoding
  let original_str = "Hello, World!"
  let encoded_str = original_str.to_base64()
  let decoded_str = encoded_str.from_base64()
  assert_eq(original_str, decoded_str)
}

// Test 5: Time and Date Conversion
test "time and date conversion" {
  // Timestamp to Date conversion
  let timestamp = 1640995200 // 2022-01-01 00:00:00 UTC
  let date_obj = timestamp.to_date()
  assert_eq(date_obj.year, 2022)
  assert_eq(date_obj.month, 1)
  assert_eq(date_obj.day, 1)
  
  // Date to Timestamp conversion
  let date_to_convert = {
    year: 2023,
    month: 12,
    day: 25,
    hour: 15,
    minute: 30,
    second: 45
  }
  let converted_timestamp = date_to_convert.to_timestamp()
  assert_eq(converted_timestamp, 1703505045)
  
  // Date to String conversion
  let date_str = date_obj.to_string("YYYY-MM-DD")
  assert_eq(date_str, "2022-01-01")
  
  // String to Date conversion
  let date_str2 = "2023-12-25"
  let parsed_date = date_str2.parse_date("YYYY-MM-DD")
  match parsed_date {
    Some(date) => {
      assert_eq(date.year, 2023)
      assert_eq(date.month, 12)
      assert_eq(date.day, 25)
    }
    None => assert_true(false)
  }
}

// Test 6: Unit Conversion
test "unit conversion" {
  // Temperature conversion
  let celsius = 25.0
  let fahrenheit = celsius.to_fahrenheit()
  assert_eq(fahrenheit, 77.0)
  
  let fahrenheit2 = 98.6
  let celsius2 = fahrenheit2.to_celsius()
  assert_eq(celsius2, 37.0)
  
  // Distance conversion
  let kilometers = 5.0
  let miles = kilometers.to_miles()
  assert_eq(miles, 3.10686)
  
  let miles2 = 10.0
  let kilometers2 = miles2.to_kilometers()
  assert_eq(kilometers2, 16.0934)
  
  // Weight conversion
  let kilograms = 70.0
  let pounds = kilograms.to_pounds()
  assert_eq(pounds, 154.324)
  
  let pounds2 = 150.0
  let kilograms2 = pounds2.to_kilograms()
  assert_eq(kilograms2, 68.0389)
}

// Test 7: Data Structure Conversion
test "data structure conversion" {
  // Map to List of Pairs conversion
  let map_data = {
    "key1": "value1",
    "key2": "value2",
    "key3": "value3"
  }
  let pair_list = map_data.to_pair_list()
  assert_eq(pair_list.length(), 3)
  
  // List of Pairs to Map conversion
  let pair_list2 = [
    ("name", "Alice"),
    ("age", "30"),
    ("city", "New York")
  ]
  let map_data2 = pair_list2.to_map()
  match map_data2.get("name") {
    Some(value) => assert_eq(value, "Alice")
    None => assert_true(false)
  }
  match map_data2.get("age") {
    Some(value) => assert_eq(value, "30")
    None => assert_true(false)
  }
  
  // Set to Array conversion
  let set_data = Set::from_array([1, 2, 3, 4, 5])
  let array_from_set = set_data.to_array()
  assert_eq(array_from_set.length(), 5)
  
  // Array to Set conversion
  let array_with_duplicates = [1, 2, 2, 3, 3, 3, 4, 5]
  let set_from_array = Set::from_array(array_with_duplicates)
  assert_eq(set_from_array.size(), 5)
}

// Test 8: Advanced Data Conversion
test "advanced data conversion" {
  // Binary to Hexadecimal conversion
  let binary_data = [0x48, 0x65, 0x6C, 0x6C, 0x6F] // "Hello" in ASCII
  let hex_string = binary_data.to_hex()
  assert_eq(hex_string, "48656C6C6F")
  
  // Hexadecimal to Binary conversion
  let hex_string2 = "576F726C64" // "World" in hex
  let binary_data2 = hex_string2.from_hex()
  let decoded_str = binary_data2.to_string()
  assert_eq(decoded_str, "World")
  
  // URL Encoding/Decoding
  let url_to_encode = "https://example.com/search?q=hello world"
  let encoded_url = url_to_encode.url_encode()
  let decoded_url = encoded_url.url_decode()
  assert_eq(url_to_encode, decoded_url)
  
  // Case conversion
  let mixed_case = "Hello World"
  let upper_case = mixed_case.to_upper()
  let lower_case = mixed_case.to_lower()
  let title_case = lower_case.to_title()
  
  assert_eq(upper_case, "HELLO WORLD")
  assert_eq(lower_case, "hello world")
  assert_eq(title_case, "Hello World")
}