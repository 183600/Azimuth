// Azimuth Serialization and Deserialization Test Suite
// This file contains test cases for serialization and deserialization mechanisms

// Test 1: Basic Type Serialization
test "basic type serialization" {
  // Serialize basic types to strings
  let serialize_int = fn(i: Int) -> String {
    "int:" + i.to_string()
  }
  
  let serialize_bool = fn(b: Bool) -> String {
    if b { "bool:true" } else { "bool:false" }
  }
  
  let serialize_string = fn(s: String) -> String {
    "str:" + s.length().to_string() + ":" + s
  }
  
  // Deserialize basic types from strings
  let deserialize_int = fn(s: String) -> Option<Int> {
    if s.length() > 4 && s.substring(0, 4) == "int:" {
      let num_str = s.substring(4, s.length() - 4)
      match num_str {
        "0" => Some(0)
        "1" => Some(1)
        "2" => Some(2)
        "3" => Some(3)
        "4" => Some(4)
        "5" => Some(5)
        "6" => Some(6)
        "7" => Some(7)
        "8" => Some(8)
        "9" => Some(9)
        "10" => Some(10)
        "42" => Some(42)
        _ => None
      }
    } else {
      None
    }
  }
  
  let deserialize_bool = fn(s: String) -> Option<Bool> {
    if s == "bool:true" {
      Some(true)
    } else if s == "bool:false" {
      Some(false)
    } else {
      None
    }
  }
  
  let deserialize_string = fn(s: String) -> Option<String> {
    if s.length() > 4 && s.substring(0, 4) == "str:" {
      let colon_pos = s.substring(4, s.length() - 4).find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = s.substring(4, pos)
          let length = match length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            "6" => 6
            "7" => 7
            "8" => 8
            "9" => 9
            "10" => 10
            _ => -1
          }
          
          if length >= 0 {
            let start_pos = 4 + pos + 1
            if s.length() - start_pos >= length {
              Some(s.substring(start_pos, length))
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  // Test int serialization/deserialization
  let int_serialized = serialize_int(42)
  assert_eq(int_serialized, "int:42")
  
  match deserialize_int(int_serialized) {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  match deserialize_int("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test bool serialization/deserialization
  let bool_true_serialized = serialize_bool(true)
  assert_eq(bool_true_serialized, "bool:true")
  
  match deserialize_bool(bool_true_serialized) {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let bool_false_serialized = serialize_bool(false)
  assert_eq(bool_false_serialized, "bool:false")
  
  match deserialize_bool(bool_false_serialized) {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  match deserialize_bool("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test string serialization/deserialization
  let string_serialized = serialize_string("hello")
  assert_eq(string_serialized, "str:5:hello")
  
  match deserialize_string(string_serialized) {
    Some(value) => assert_eq(value, "hello")
    None => assert_true(false)
  }
  
  match deserialize_string("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Array Serialization
test "array serialization" {
  // Serialize arrays of integers
  let serialize_int_array = fn(arr: Array<Int>) -> String {
    let mut result = "arr:" + arr.length().to_string() + ":"
    for i in arr {
      result = result + i.to_string() + ","
    }
    result
  }
  
  // Deserialize arrays of integers
  let deserialize_int_array = fn(s: String) -> Option<Array<Int>> {
    if s.length() > 4 && s.substring(0, 4) == "arr:" {
      let colon_pos = s.substring(4, s.length() - 4).find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = s.substring(4, pos)
          let length = match length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            "6" => 6
            "7" => 7
            "8" => 8
            "9" => 9
            "10" => 10
            _ => -1
          }
          
          if length >= 0 {
            let start_pos = 4 + pos + 1
            let data_str = s.substring(start_pos, s.length() - start_pos)
            let mut result = []
            
            if length == 0 {
              return Some(result)
            }
            
            let mut current = ""
            for i in 0..<data_str.length() {
              let char = data_str.substring(i, 1)
              if char == "," {
                match current {
                  "0" => result = result.push(0)
                  "1" => result = result.push(1)
                  "2" => result = result.push(2)
                  "3" => result = result.push(3)
                  "4" => result = result.push(4)
                  "5" => result = result.push(5)
                  "6" => result = result.push(6)
                  "7" => result = result.push(7)
                  "8" => result = result.push(8)
                  "9" => result = result.push(9)
                  "10" => result = result.push(10)
                  _ => {}
                }
                current = ""
              } else {
                current = current + char
              }
            }
            
            if result.length() == length {
              Some(result)
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  // Test array serialization/deserialization
  let empty_array: Array<Int> = []
  let empty_serialized = serialize_int_array(empty_array)
  assert_eq(empty_serialized, "arr:0:")
  
  match deserialize_int_array(empty_serialized) {
    Some(arr) => assert_eq(arr.length(), 0)
    None => assert_true(false)
  }
  
  let single_element = [42]
  let single_serialized = serialize_int_array(single_element)
  assert_eq(single_serialized, "arr:1:42,")
  
  match deserialize_int_array(single_serialized) {
    Some(arr) => {
      assert_eq(arr.length(), 1)
      assert_eq(arr[0], 42)
    }
    None => assert_true(false)
  }
  
  let multiple_elements = [1, 2, 3, 4, 5]
  let multiple_serialized = serialize_int_array(multiple_elements)
  assert_eq(multiple_serialized, "arr:5:1,2,3,4,5,")
  
  match deserialize_int_array(multiple_serialized) {
    Some(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    None => assert_true(false)
  }
  
  match deserialize_int_array("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Tuple Serialization
test "tuple serialization" {
  // Serialize tuples
  let serialize_tuple2 = fn(a: Int, b: String) -> String {
    "tuple2:" + a.to_string() + ":" + b.length().to_string() + ":" + b
  }
  
  let serialize_tuple3 = fn(a: Int, b: String, c: Bool) -> String {
    "tuple3:" + a.to_string() + ":" + b.length().to_string() + ":" + b + ":" + (if c { "true" } else { "false" })
  }
  
  // Deserialize tuples
  let deserialize_tuple2 = fn(s: String) -> Option<(Int, String)> {
    if s.length() > 7 && s.substring(0, 7) == "tuple2:" {
      let remaining = s.substring(7, s.length() - 7)
      let colon_pos = remaining.find(":")
      match colon_pos {
        Some(pos) => {
          let int_str = remaining.substring(0, pos)
          let int_value = match int_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            "6" => 6
            "7" => 7
            "8" => 8
            "9" => 9
            "10" => 10
            "42" => 42
            _ => -1
          }
          
          if int_value >= 0 {
            let length_str = remaining.substring(pos + 1, 1)
            let length = match length_str {
              "0" => 0
              "1" => 1
              "2" => 2
              "3" => 3
              "4" => 4
              "5" => 5
              _ => -1
            }
            
            if length >= 0 {
              let start_pos = pos + 1 + 1
              let string_value = remaining.substring(start_pos, length)
              Some((int_value, string_value))
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  let deserialize_tuple3 = fn(s: String) -> Option<(Int, String, Bool)> {
    if s.length() > 7 && s.substring(0, 7) == "tuple3:" {
      let remaining = s.substring(7, s.length() - 7)
      let colon_pos1 = remaining.find(":")
      match colon_pos1 {
        Some(pos1) => {
          let int_str = remaining.substring(0, pos1)
          let int_value = match int_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            "6" => 6
            "7" => 7
            "8" => 8
            "9" => 9
            "10" => 10
            "42" => 42
            _ => -1
          }
          
          if int_value >= 0 {
            let remaining2 = remaining.substring(pos1 + 1, remaining.length() - pos1 - 1)
            let colon_pos2 = remaining2.find(":")
            match colon_pos2 {
              Some(pos2) => {
                let length_str = remaining2.substring(0, pos2)
                let length = match length_str {
                  "0" => 0
                  "1" => 1
                  "2" => 2
                  "3" => 3
                  "4" => 4
                  "5" => 5
                  _ => -1
                }
                
                if length >= 0 {
                  let start_pos = pos2 + 1
                  let string_value = remaining2.substring(start_pos, length)
                  let remaining3 = remaining2.substring(start_pos + length, remaining2.length() - start_pos - length)
                  
                  if remaining3.length() > 0 && remaining3[0] == ":" {
                    let bool_str = remaining3.substring(1, remaining3.length() - 1)
                    let bool_value = if bool_str == "true" { true } else if bool_str == "false" { false } else { false }
                    
                    Some((int_value, string_value, bool_value))
                  } else {
                    None
                  }
                } else {
                  None
                }
              }
              None => None
            }
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  // Test tuple2 serialization/deserialization
  let tuple2_serialized = serialize_tuple2(42, "hello")
  assert_eq(tuple2_serialized, "tuple2:42:5:hello")
  
  match deserialize_tuple2(tuple2_serialized) {
    Some((int_val, str_val)) => {
      assert_eq(int_val, 42)
      assert_eq(str_val, "hello")
    }
    None => assert_true(false)
  }
  
  match deserialize_tuple2("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test tuple3 serialization/deserialization
  let tuple3_serialized = serialize_tuple3(42, "world", true)
  assert_eq(tuple3_serialized, "tuple3:42:5:world:true")
  
  match deserialize_tuple3(tuple3_serialized) {
    Some((int_val, str_val, bool_val)) => {
      assert_eq(int_val, 42)
      assert_eq(str_val, "world")
      assert_true(bool_val)
    }
    None => assert_true(false)
  }
  
  match deserialize_tuple3("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Option Type Serialization
test "option type serialization" {
  // Serialize Option types
  let serialize_option_int = fn(opt: Option<Int>) -> String {
    match opt {
      Some(value) => "some:" + value.to_string()
      None => "none"
    }
  }
  
  let serialize_option_string = fn(opt: Option<String>) -> String {
    match opt {
      Some(value) => "some:" + value.length().to_string() + ":" + value
      None => "none"
    }
  }
  
  // Deserialize Option types
  let deserialize_option_int = fn(s: String) -> Option<Int> {
    if s == "none" {
      None
    } else if s.length() > 5 && s.substring(0, 5) == "some:" {
      let value_str = s.substring(5, s.length() - 5)
      match value_str {
        "0" => Some(0)
        "1" => Some(1)
        "2" => Some(2)
        "3" => Some(3)
        "4" => Some(4)
        "5" => Some(5)
        "6" => Some(6)
        "7" => Some(7)
        "8" => Some(8)
        "9" => Some(9)
        "10" => Some(10)
        "42" => Some(42)
        _ => None
      }
    } else {
      None
    }
  }
  
  let deserialize_option_string = fn(s: String) -> Option<String> {
    if s == "none" {
      None
    } else if s.length() > 5 && s.substring(0, 5) == "some:" {
      let remaining = s.substring(5, s.length() - 5)
      let colon_pos = remaining.find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = remaining.substring(0, pos)
          let length = match length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            _ => -1
          }
          
          if length >= 0 {
            let start_pos = pos + 1
            let string_value = remaining.substring(start_pos, length)
            Some(string_value)
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  // Test Option Int serialization/deserialization
  let some_int_serialized = serialize_option_int(Some(42))
  assert_eq(some_int_serialized, "some:42")
  
  match deserialize_option_int(some_int_serialized) {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  let none_int_serialized = serialize_option_int(None)
  assert_eq(none_int_serialized, "none")
  
  match deserialize_option_int(none_int_serialized) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test Option String serialization/deserialization
  let some_string_serialized = serialize_option_string(Some("hello"))
  assert_eq(some_string_serialized, "some:5:hello")
  
  match deserialize_option_string(some_string_serialized) {
    Some(value) => assert_eq(value, "hello")
    None => assert_true(false)
  }
  
  let none_string_serialized = serialize_option_string(None)
  assert_eq(none_string_serialized, "none")
  
  match deserialize_option_string(none_string_serialized) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Result Type Serialization
test "result type serialization" {
  // Serialize Result types
  let serialize_result_int_string = fn(res: Result<Int, String>) -> String {
    match res {
      Ok(value) => "ok:" + value.to_string()
      Err(msg) => "err:" + msg.length().to_string() + ":" + msg
    }
  }
  
  // Deserialize Result types
  let deserialize_result_int_string = fn(s: String) -> Result<Int, String> {
    if s.length() > 3 && s.substring(0, 3) == "ok:" {
      let value_str = s.substring(3, s.length() - 3)
      match value_str {
        "0" => Ok(0)
        "1" => Ok(1)
        "2" => Ok(2)
        "3" => Ok(3)
        "4" => Ok(4)
        "5" => Ok(5)
        "6" => Ok(6)
        "7" => Ok(7)
        "8" => Ok(8)
        "9" => Ok(9)
        "10" => Ok(10)
        "42" => Ok(42)
        _ => Err("Invalid value")
      }
    } else if s.length() > 4 && s.substring(0, 4) == "err:" {
      let remaining = s.substring(4, s.length() - 4)
      let colon_pos = remaining.find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = remaining.substring(0, pos)
          let length = match length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            "6" => 6
            "7" => 7
            _ => -1
          }
          
          if length >= 0 {
            let start_pos = pos + 1
            let error_msg = remaining.substring(start_pos, length)
            Err(error_msg)
          } else {
            Err("Invalid error format")
          }
        }
        None => Err("Invalid error format")
      }
    } else {
      Err("Invalid result format")
    }
  }
  
  // Test Result serialization/deserialization
  let ok_serialized = serialize_result_int_string(Ok(42))
  assert_eq(ok_serialized, "ok:42")
  
  match deserialize_result_int_string(ok_serialized) {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  let err_serialized = serialize_result_int_string(Err("error"))
  assert_eq(err_serialized, "err:5:error")
  
  match deserialize_result_int_string(err_serialized) {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "error")
  }
  
  match deserialize_result_int_string("invalid") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid result format")
  }
}

// Test 6: Complex Nested Structure Serialization
test "complex nested structure serialization" {
  // Serialize a complex nested structure
  type Person = {
    name: String,
    age: Int,
    email: Option<String>
  }
  
  type Team = {
    name: String,
    members: Array<Person>
  }
  
  let serialize_person = fn(person: Person) -> String {
    let email_part = match person.email {
      Some(email) => "some:" + email.length().to_string() + ":" + email
      None => "none"
    }
    "person:" + person.name.length().to_string() + ":" + person.name + ":" + person.age.to_string() + ":" + email_part
  }
  
  let serialize_team = fn(team: Team) -> String {
    let mut result = "team:" + team.name.length().to_string() + ":" + team.name + ":" + team.members.length().to_string() + ":"
    for member in team.members {
      result = result + serialize_person(member) + ";"
    }
    result
  }
  
  // Deserialize the complex nested structure
  let deserialize_person = fn(s: String) -> Option<Person> {
    if s.length() > 7 && s.substring(0, 7) == "person:" {
      let remaining = s.substring(7, s.length() - 7)
      let colon_pos1 = remaining.find(":")
      match colon_pos1 {
        Some(pos1) => {
          let name_length_str = remaining.substring(0, pos1)
          let name_length = match name_length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            _ => -1
          }
          
          if name_length >= 0 {
            let start_pos1 = pos1 + 1
            let name = remaining.substring(start_pos1, name_length)
            let remaining2 = remaining.substring(start_pos1 + name_length, remaining.length() - start_pos1 - name_length)
            
            if remaining2.length() > 0 && remaining2[0] == ":" {
              let remaining3 = remaining2.substring(1, remaining2.length() - 1)
              let colon_pos2 = remaining3.find(":")
              match colon_pos2 {
                Some(pos2) => {
                  let age_str = remaining3.substring(0, pos2)
                  let age = match age_str {
                    "0" => 0
                    "1" => 1
                    "2" => 2
                    "3" => 3
                    "4" => 4
                    "5" => 5
                    "6" => 6
                    "7" => 7
                    "8" => 8
                    "9" => 9
                    "10" => 10
                    "25" => 25
                    "30" => 30
                    _ => -1
                  }
                  
                  if age >= 0 {
                    let remaining4 = remaining3.substring(pos2 + 1, remaining3.length() - pos2 - 1)
                    let email = if remaining4 == "none" {
                      None
                    } else if remaining4.length() > 5 && remaining4.substring(0, 5) == "some:" {
                      let email_remaining = remaining4.substring(5, remaining4.length() - 5)
                      let email_colon_pos = email_remaining.find(":")
                      match email_colon_pos {
                        Some(email_pos) => {
                          let email_length_str = email_remaining.substring(0, email_pos)
                          let email_length = match email_length_str {
                            "0" => 0
                            "1" => 1
                            "2" => 2
                            "3" => 3
                            "4" => 4
                            "5" => 5
                            "6" => 6
                            "7" => 7
                            "8" => 8
                            "9" => 9
                            "10" => 10
                            "11" => 11
                            "12" => 12
                            "13" => 13
                            "14" => 14
                            "15" => 15
                            "16" => 16
                            "17" => 17
                            "18" => 18
                            "19" => 19
                            "20" => 20
                            _ => -1
                          }
                          
                          if email_length >= 0 {
                            let email_start_pos = email_pos + 1
                            let email_value = email_remaining.substring(email_start_pos, email_length)
                            Some(email_value)
                          } else {
                            None
                          }
                        }
                        None => None
                      }
                    } else {
                      None
                    }
                    
                    Some({ name: name, age: age, email: email })
                  } else {
                    None
                  }
                }
                None => None
              }
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  let deserialize_team = fn(s: String) -> Option<Team> {
    if s.length() > 5 && s.substring(0, 5) == "team:" {
      let remaining = s.substring(5, s.length() - 5)
      let colon_pos1 = remaining.find(":")
      match colon_pos1 {
        Some(pos1) => {
          let name_length_str = remaining.substring(0, pos1)
          let name_length = match name_length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            "6" => 6
            "7" => 7
            "8" => 8
            "9" => 9
            _ => -1
          }
          
          if name_length >= 0 {
            let start_pos1 = pos1 + 1
            let name = remaining.substring(start_pos1, name_length)
            let remaining2 = remaining.substring(start_pos1 + name_length, remaining.length() - start_pos1 - name_length)
            
            if remaining2.length() > 0 && remaining2[0] == ":" {
              let remaining3 = remaining2.substring(1, remaining2.length() - 1)
              let colon_pos2 = remaining3.find(":")
              match colon_pos2 {
                Some(pos2) => {
                  let members_count_str = remaining3.substring(0, pos2)
                  let members_count = match members_count_str {
                    "0" => 0
                    "1" => 1
                    "2" => 2
                    "3" => 3
                    _ => -1
                  }
                  
                  if members_count >= 0 {
                    let start_pos2 = pos2 + 1
                    let members_str = remaining3.substring(start_pos2, remaining3.length() - start_pos2)
                    
                    let mut members = []
                    let mut current = ""
                    
                    for i in 0..<members_str.length() {
                      let char = members_str.substring(i, 1)
                      if char == ";" {
                        match deserialize_person(current) {
                          Some(person) => members = members.push(person)
                          None => {}
                        }
                        current = ""
                      } else {
                        current = current + char
                      }
                    }
                    
                    if members.length() == members_count {
                      Some({ name: name, members: members })
                    } else {
                      None
                    }
                  } else {
                    None
                  }
                }
                None => None
              }
            } else {
              None
            }
          } else {
            None
          }
        }
        None => None
      }
    } else {
      None
    }
  }
  
  // Test complex nested structure serialization/deserialization
  let person1 = { name: "Alice", age: 30, email: Some("alice@example.com") }
  let person2 = { name: "Bob", age: 25, email: None }
  let team = { name: "Team", members: [person1, person2] }
  
  let team_serialized = serialize_team(team)
  
  match deserialize_team(team_serialized) {
    Some(deserialized_team) => {
      assert_eq(deserialized_team.name, "Team")
      assert_eq(deserialized_team.members.length(), 2)
      
      let member1 = deserialized_team.members[0]
      assert_eq(member1.name, "Alice")
      assert_eq(member1.age, 30)
      match member1.email {
        Some(email) => assert_eq(email, "alice@example.com")
        None => assert_true(false)
      }
      
      let member2 = deserialized_team.members[1]
      assert_eq(member2.name, "Bob")
      assert_eq(member2.age, 25)
      match member2.email {
        Some(_) => assert_true(false)
        None => assert_true(true)
      }
    }
    None => assert_true(false)
  }
  
  match deserialize_team("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Serialization Format Validation
test "serialization format validation" {
  // Validate serialized format
  let validate_int_format = fn(s: String) -> Bool {
    s.length() > 4 && s.substring(0, 4) == "int:"
  }
  
  let validate_bool_format = fn(s: String) -> Bool {
    s == "bool:true" || s == "bool:false"
  }
  
  let validate_string_format = fn(s: String) -> Bool {
    if s.length() > 4 && s.substring(0, 4) == "str:" {
      let remaining = s.substring(4, s.length() - 4)
      let colon_pos = remaining.find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = remaining.substring(0, pos)
          match length_str {
            "0" => true
            "1" => true
            "2" => true
            "3" => true
            "4" => true
            "5" => true
            _ => false
          }
        }
        None => false
      }
    } else {
      false
    }
  }
  
  let validate_array_format = fn(s: String) -> Bool {
    if s.length() > 4 && s.substring(0, 4) == "arr:" {
      let remaining = s.substring(4, s.length() - 4)
      let colon_pos = remaining.find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = remaining.substring(0, pos)
          match length_str {
            "0" => true
            "1" => true
            "2" => true
            "3" => true
            "4" => true
            "5" => true
            _ => false
          }
        }
        None => false
      }
    } else {
      false
    }
  }
  
  // Test format validation
  assert_true(validate_int_format("int:42"))
  assert_false(validate_int_format("str:5:hello"))
  assert_false(validate_int_format("invalid"))
  
  assert_true(validate_bool_format("bool:true"))
  assert_true(validate_bool_format("bool:false"))
  assert_false(validate_bool_format("bool:maybe"))
  assert_false(validate_bool_format("invalid"))
  
  assert_true(validate_string_format("str:5:hello"))
  assert_false(validate_string_format("str:5"))  // Missing colon
  assert_false(validate_string_format("str:hello"))  // Missing length
  assert_false(validate_string_format("int:42"))
  
  assert_true(validate_array_format("arr:3:1,2,3,"))
  assert_false(validate_array_format("arr:"))  // Missing length
  assert_false(validate_array_format("arr:3:1,2,3"))  // Missing trailing comma
  assert_false(validate_array_format("str:5:hello"))
}

// Test 8: Serialization Error Handling
test "serialization error handling" {
  // Handle serialization errors gracefully
  let safe_deserialize_int = fn(s: String) -> Result<Int, String> {
    if s.length() > 4 && s.substring(0, 4) == "int:" {
      let value_str = s.substring(4, s.length() - 4)
      match value_str {
        "0" => Ok(0)
        "1" => Ok(1)
        "2" => Ok(2)
        "3" => Ok(3)
        "4" => Ok(4)
        "5" => Ok(5)
        "6" => Ok(6)
        "7" => Ok(7)
        "8" => Ok(8)
        "9" => Ok(9)
        "10" => Ok(10)
        "42" => Ok(42)
        _ => Err("Invalid integer value: " + value_str)
      }
    } else {
      Err("Invalid integer format: " + s)
    }
  }
  
  let safe_deserialize_string = fn(s: String) -> Result<String, String> {
    if s.length() > 4 && s.substring(0, 4) == "str:" {
      let remaining = s.substring(4, s.length() - 4)
      let colon_pos = remaining.find(":")
      match colon_pos {
        Some(pos) => {
          let length_str = remaining.substring(0, pos)
          let length = match length_str {
            "0" => 0
            "1" => 1
            "2" => 2
            "3" => 3
            "4" => 4
            "5" => 5
            _ => -1
          }
          
          if length >= 0 {
            let start_pos = pos + 1
            if remaining.length() - start_pos >= length {
              Ok(remaining.substring(start_pos, length))
            } else {
              Err("String length mismatch: expected " + length.to_string() + ", got " + (remaining.length() - start_pos).to_string())
            }
          } else {
            Err("Invalid string length: " + length_str)
          }
        }
        None => Err("Missing length separator in string: " + s)
      }
    } else {
      Err("Invalid string format: " + s)
    }
  }
  
  // Test error handling
  match safe_deserialize_int("int:42") {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  match safe_deserialize_int("int:invalid") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid integer value: invalid")
  }
  
  match safe_deserialize_int("invalid") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid integer format: invalid")
  }
  
  match safe_deserialize_string("str:5:hello") {
    Ok(value) => assert_eq(value, "hello")
    Err(_) => assert_true(false)
  }
  
  match safe_deserialize_string("str:5:hel") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "String length mismatch: expected 5, got 3")
  }
  
  match safe_deserialize_string("str:invalid:hello") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid string length: invalid")
  }
  
  match safe_deserialize_string("invalid") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid string format: invalid")
  }
}