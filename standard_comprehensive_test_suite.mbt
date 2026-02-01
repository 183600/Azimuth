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

// 标准 MoonBit 测试套件 - 包含8个高质量测试用例

///|
test "array_zip_with_functionality" {
  let arr1 = [1, 2, 3, 4]
  let arr2 = [10, 20, 30, 40]
  let add = (a : Int, b : Int) => a + b
  let result = @array.zip_with(arr1, arr2, add)
  inspect(result, content="[11, 22, 33, 44]")
  
  // Test with different length arrays
  let short_arr1 = [1, 2]
  let long_arr2 = [10, 20, 30, 40]
  let result2 = @array.zip_with(short_arr1, long_arr2, add)
  inspect(result2, content="[11, 22]")
  
  // Test with string concatenation
  let words1 = ["Hello", "Good"]
  let words2 = ["World", "Morning"]
  let concat = (a : String, b : String) => a + " " + b
  let result3 = @array.zip_with(words1, words2, concat)
  inspect(result3, content="[\"Hello World\", \"Good Morning\"]")
}

///|
test "string_unicode_operations" {
  let chinese_text = "你好，世界"
  let english_text = "Hello, World"
  let mixed_text = "Hello, 世界"
  
  // Test string lengths
  inspect(chinese_text.length(), content="5")
  inspect(english_text.length(), content="12")
  inspect(mixed_text.length(), content="9")
  
  // Test character access
  inspect(chinese_text.char_at(0), content="'你'")
  inspect(mixed_text.char_at(7), content="'界'")
  
  // Test string concatenation with unicode
  let combined = chinese_text + " - " + english_text
  inspect(combined, content="你好，世界 - Hello, World")
}

///|
test "option_advanced_operations" {
  let numbers = [Some(1), None, Some(3), None, Some(5)]
  
  // Filter out None values
  let filtered = numbers.filter(opt => opt.is_some())
  inspect(filtered.length(), content="3")
  
  // Extract values from Some
  let values = filtered.map(opt => opt.unwrap())
  inspect(values, content="[1, 3, 5]")
  
  // Chain operations
  let result = Some(42)
    .map(x => x * 2)
    .filter(x => x > 50)
    .map(x => x / 2)
  
  inspect(result, content="Some(42)")
  
  // Test with None chain
  let none_result = None
    .map((x : Int) => x * 2)
    .filter(x => x > 50)
  
  inspect(none_result, content="None")
}

///|
test "int_complex_arithmetic" {
  // Test power operations
  inspect(Int::pow(2, 8), content="256")
  inspect(Int::pow(3, 4), content="81")
  inspect(Int::pow(5, 3), content="125")
  
  // Test bit manipulation
  let value = 0b10101010  // 170 in decimal
  inspect(value & 0b11110000, content="160")  // Extract high bits
  inspect(value | 0b00001111, content="175")  // Set low bits
  inspect(value ^ 0b11111111, content="85")   // Invert all bits
  
  // Test shift operations with multiplication
  inspect(5 << 3, content="40")  // 5 * 2^3
  inspect(40 >> 3, content="5")  // 40 / 2^3
}

///|
test "result_error_propagation" {
  let safe_divide = (a : Int, b : Int) -> Result[Int, String] {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  // Test successful operations
  let result1 = safe_divide(10, 2)
  inspect(result1, content="Ok(5)")
  
  let chained = result1.map(x => x * 3)
  inspect(chained, content="Ok(15)")
  
  // Test error propagation
  let result2 = safe_divide(10, 0)
  inspect(result2, content="Err(\"Division by zero\")")
  
  let chained_err = result2.map(x => x * 3)
  inspect(chained_err, content="Err(\"Division by zero\")")
  
  // Test and_then for chaining operations that might fail
  let final_result = result1.and_then(x => safe_divide(x, 5))
  inspect(final_result, content="Ok(1)")
}

///|
test "float_precision_operations" {
  // Test floating point arithmetic precision
  let pi = @math.pi
  let e = @math.e
  
  inspect(pi > 3.14 && pi < 3.15, content="true")
  inspect(e > 2.71 && e < 2.72, content="true")
  
  // Test trigonometric functions
  let sin_pi = @math.sinf(pi)
  let cos_pi = @math.cosf(pi)
  
  // sin(π) should be very close to 0
  assert_true(sin_pi.abs() < 0.0001)
  // cos(π) should be very close to -1
  assert_true((cos_pi + 1.0).abs() < 0.0001)
  
  // Test square root and power
  inspect(@math.sqrtf(16.0), content="4")
  inspect(@math.powf(2.0, 10.0), content="1024")
}

///|
test "char_classification_and_conversion" {
  let chars = ['A', 'z', '0', '9', ' ', '!', '中']
  
  // Test character to string conversion
  inspect(chars[0].to_string(), content="A")
  inspect(chars[6].to_string(), content="中")
  
  // Test character comparisons
  inspect('A' < 'B', content="true")
  inspect('a' > 'Z', content="true")
  inspect('0' < '9', content="true")
  
  // Test with ASCII values
  let uppercase_A = 'A'.to_int()
  let lowercase_a = 'a'.to_int()
  
  inspect(lowercase_a - uppercase_A, content="32")  // ASCII difference
  
  // Test character arithmetic
  let next_char = ('A'.to_int() + 1).to_char()
  inspect(next_char.to_string(), content="B")
}

///|
test "array_functional_composition" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Complex functional pipeline
  let result = numbers
    .filter(x => x % 2 == 0)  // Even numbers only
    .map(x => x * x)          // Square each
    .filter(x => x > 10)      // Greater than 10
    .fold(0, (acc, x) => acc + x)  // Sum them up
  
  inspect(result, content="220")  // 4² + 6² + 8² + 10² = 16 + 36 + 64 + 100 = 216
  
  // Test with string operations
  let words = ["apple", "banana", "cherry", "date"]
  let word_result = words
    .filter(w => w.length() > 5)
    .map(w => w.to_uppercase())
    .fold("", (acc, w) => acc + w + " ")
  
  inspect(word_result.trim(), content="BANANA CHERRY")
}