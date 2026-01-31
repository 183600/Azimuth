// Copyright 2026 International Digital Economy Academy
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

///|
test "hashmap basic operations" {
  let map = @hashmap.new[Int, String]()
  
  assert_eq(map.length(), 0)
  assert_eq(map.is_empty(), true)
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  assert_eq(map.length(), 3)
  assert_eq(map.is_empty(), false)
}

///|
test "hashmap insert and get operations" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  assert_eq(map.get(1), Some("one"))
  assert_eq(map.get(2), Some("two"))
  assert_eq(map.get(3), Some("three"))
  assert_eq(map.get(4), None[String])
  
  // Test overwriting existing key
  map.insert(1, "ONE")
  assert_eq(map.get(1), Some("ONE"))
  assert_eq(map.length(), 3)  // Length should remain the same
}

///|
test "hashmap remove operations" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  let removed = map.remove(2)
  assert_eq(removed, Some("two"))
  assert_eq(map.get(2), None[String])
  assert_eq(map.length(), 2)
  
  let not_found = map.remove(4)
  assert_eq(not_found, None[String])
  assert_eq(map.length(), 2)
}

///|
test "hashmap contains_key operations" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  assert_eq(map.contains_key(1), true)
  assert_eq(map.contains_key(2), true)
  assert_eq(map.contains_key(3), true)
  assert_eq(map.contains_key(4), false)
}

///|
test "hashmap get_or operations" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  
  assert_eq(map.get_or(1, "default"), "one")
  assert_eq(map.get_or(2, "default"), "two")
  assert_eq(map.get_or(3, "default"), "default")
  
  // Test get_or_with
  assert_eq(map.get_or_with(1, || "computed"), "one")
  assert_eq(map.get_or_with(3, || "computed"), "computed")
}

///|
test "hashmap entry operations" {
  let map = @hashmap.new[Int, String]()
  
  // Test or_insert
  let value1 = map.entry(1).or_insert("one")
  assert_eq(value1, "one")
  assert_eq(map.get(1), Some("one"))
  assert_eq(map.length(), 1)
  
  // Test or_insert with existing key
  let value1_existing = map.entry(1).or_insert("ONE")
  assert_eq(value1_existing, "one")  // Should return existing value
  assert_eq(map.get(1), Some("one"))  // Value should not change
  
  // Test or_insert_with
  let value2 = map.entry(2).or_insert_with(|| "two")
  assert_eq(value2, "two")
  assert_eq(map.get(2), Some("two"))
  assert_eq(map.length(), 2)
}

///|
test "hashmap iteration" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  let mut key_sum = 0
  let mut value_concat = ""
  
  for (key, value) in map {
    key_sum = key_sum + key
    value_concat = value_concat + value
  }
  
  assert_eq(key_sum, 6)  // 1 + 2 + 3
  assert_eq(value_concat, "onetwothree")  // Order may vary, but all values should be present
}

///|
test "hashmap keys and values iteration" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  let mut key_sum = 0
  for key in map.keys() {
    key_sum = key_sum + key
  }
  assert_eq(key_sum, 6)
  
  let mut value_concat = ""
  for value in map.values() {
    value_concat = value_concat + value
  }
  assert_eq(value_concat.length(), 11)  // "one"(3) + "two"(3) + "three"(5) = 11
}

///|
test "hashmap clear operation" {
  let map = @hashmap.new[Int, String]()
  
  map.insert(1, "one")
  map.insert(2, "two")
  map.insert(3, "three")
  
  assert_eq(map.length(), 3)
  assert_eq(map.is_empty(), false)
  
  map.clear()
  
  assert_eq(map.length(), 0)
  assert_eq(map.is_empty(), true)
  assert_eq(map.get(1), None[String])
}

///|
test "hashmap with different key types" {
  // Test with string keys
  let string_map = @hashmap.new[String, Int]()
  
  string_map.insert("one", 1)
  string_map.insert("two", 2)
  string_map.insert("three", 3)
  
  assert_eq(string_map.get("one"), Some(1))
  assert_eq(string_map.get("two"), Some(2))
  assert_eq(string_map.get("three"), Some(3))
  assert_eq(string_map.get("four"), None[Int])
  
  // Test with tuple keys
  let tuple_map = @hashmap.new[(Int, String), String]()
  
  tuple_map.insert((1, "first"), "value1")
  tuple_map.insert((2, "second"), "value2")
  
  assert_eq(tuple_map.get((1, "first")), Some("value1"))
  assert_eq(tuple_map.get((2, "second")), Some("value2"))
  assert_eq(tuple_map.get((3, "third")), None[String])
}

///|
test "hashmap resize behavior" {
  let map = @hashmap.new[Int, Int]()
  
  // Insert many elements to trigger resizing
  for i = 0; i < 1000; i = i + 1 {
    map.insert(i, i * 2)
  }
  
  assert_eq(map.length(), 1000)
  
  // Verify some values
  assert_eq(map.get(0), Some(0))
  assert_eq(map.get(500), Some(1000))
  assert_eq(map.get(999), Some(1998))
  assert_eq(map.get(1000), None[Int])
  
  // Remove some elements
  for i = 0; i < 500; i = i + 1 {
    map.remove(i)
  }
  
  assert_eq(map.length(), 500)
  assert_eq(map.get(0), None[Int])
  assert_eq(map.get(500), Some(1000))
}

///|
test "hashmap equality" {
  let map1 = @hashmap.new[Int, String]()
  let map2 = @hashmap.new[Int, String]()
  let map3 = @hashmap.new[Int, String]()
  
  map1.insert(1, "one")
  map1.insert(2, "two")
  
  map2.insert(1, "one")
  map2.insert(2, "two")
  
  map3.insert(1, "one")
  map3.insert(2, "TWO")  // Different value
  
  assert_eq(map1 == map2, true)
  assert_eq(map1 == map3, false)
  assert_eq(map2 == map3, false)
  
  assert_eq(map1 != map2, false)
  assert_eq(map1 != map3, true)
  assert_eq(map2 != map3, true)
}