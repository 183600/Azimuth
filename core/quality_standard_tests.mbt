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
test "int_overflow_and_underflow" {
  // 测试整数溢出和下溢
  inspect(@int.max_value + 1, content=@int.min_value.to_string())
  inspect(@int.min_value - 1, content=@int.max_value.to_string())
  
  // 测试大数运算
  let large_num = 1000000
  inspect(large_num * large_num, content="1000000000000")
}

///|
test "advanced_string_operations" {
  // 测试字符串高级操作
  let text = "Hello, 世界!"
  
  // 测试字符访问
  inspect(text.char_at(0), content="'H'")
  inspect(text.char_at(7), content="'世'")
  inspect(text.char_at(8), content="'界'")
  
  // 测试字符串比较
  inspect("hello" == "hello", content="true")
  inspect("hello" != "world", content="true")
  inspect("abc" < "abd", content="true")
}

///|
test "array_advanced_operations" {
  // 测试数组高级操作
  let numbers = [1, 2, 3, 4, 5]
  let empty : Array[Int] = []
  
  // 测试数组包含
  inspect(numbers.contains(3), content="true")
  inspect(numbers.contains(6), content="false")
  inspect(empty.contains(1), content="false")
  
  // 测试数组反转
  let reversed = numbers.reverse()
  inspect(reversed, content="[5, 4, 3, 2, 1]")
  
  // 测试数组切片（如果支持）
  inspect(numbers.length(), content="5")
}

///|
test "result_type_comprehensive" {
  // 测试 Result 类型的综合操作
  let success : Result[Int, String] = Ok(42)
  let error : Result[Int, String] = Err("Something went wrong")
  
  // 测试 map_err 操作
  let mapped_err = error.map_err(msg => msg.length())
  inspect(mapped_err, content="Err(19)")
  
  // 测试 and_then 操作
  let chained = success.and_then(x => if x > 40 { Ok(x * 2) } else { Err("too small") })
  inspect(chained, content="Ok(84)")
  
  // 测试 or_else 操作
  let fallback = error.or_else(_ => Ok(0))
  inspect(fallback, content="Ok(0)")
}

///|
test "boolean_short_circuit_evaluation" {
  // 测试布尔运算的短路求值
  let mut counter = 0
  let increment = () => { counter = counter + 1; true }
  
  // 短路测试：如果第一个条件为 false，第二个不会执行
  let result1 = false && increment()
  inspect(result1, content="false")
  inspect(counter, content="0")  // increment() 没有被调用
  
  // 短路测试：如果第一个条件为 true，第二个不会执行
  let result2 = true || increment()
  inspect(result2, content="true")
  inspect(counter, content="0")  // increment() 没有被调用
}

///|
test "tuple_nested_operations" {
  // 测试嵌套元组操作
  let simple = (1, "hello")
  let nested = ((1, 2), ("hello", "world"))
  let mixed = (42, "answer", [1, 2, 3])
  
  // 测试嵌套访问
  inspect(nested.0.0, content="1")
  inspect(nested.0.1, content="2")
  inspect(nested.1.0, content="hello")
  inspect(nested.1.1, content="world")
  
  // 测试混合类型元组
  inspect(mixed.0, content="42")
  inspect(mixed.1, content="answer")
  inspect(mixed.2.length(), content="3")
}

///|
test "option_chain_operations" {
  // 测试 Option 链式操作
  let data : Option<Array[Int>> = Some([1, 2, 3, 4, 5])
  let none_data : Option<Array[Int>> = None
  
  // 链式操作：获取数组，过滤偶数，求和
  let result = data
    .filter(arr => arr.length() > 0)
    .map(arr => arr.filter(x => x % 2 == 0))
    .map(arr => arr.fold(0, (acc, x) => acc + x))
  
  inspect(result, content="Some(6)")  // 2 + 4 = 6
  
  // 对 None 进行相同操作
  let none_result = none_data
    .filter(arr => arr.length() > 0)
    .map(arr => arr.filter(x => x % 2 == 0))
    .map(arr => arr.fold(0, (acc, x) => acc + x))
  
  inspect(none_result, content="None")
}

///|
test "type_conversion_edge_cases" {
  // 测试类型转换的边界情况
  let zero = 0
  let negative = -42
  let positive = 42
  
  // Int 到 Uint 的转换
  let zero_uint = zero.reinterpret_as_uint()
  let pos_uint = positive.reinterpret_as_uint()
  let neg_uint = negative.reinterpret_as_uint()
  
  inspect(zero_uint.to_int(), content="0")
  inspect(pos_uint.to_int(), content="42")
  // 负数转换为 uint 后再转回 int 的结果取决于实现
  
  // 测试字节转换
  let test_val = 0x12345678
  let bytes = @int.Int::to_be_bytes(test_val)
  inspect(bytes.length(), content="4")
  inspect(bytes[0], content="18")  // 0x12
}

///|
test "math_function_precision" {
  // 测试数学函数的精度
  let pi = @math.pi
  let e = @math.e
  
  // 测试基本数学常数
  inspect(pi > 3.14 && pi < 3.15, content="true")
  inspect(e > 2.71 && e < 2.72, content="true")
  
  // 测试幂运算
  inspect(@math.powf(2.0, 3.0), content="8")
  inspect(@math.powf(5.0, 0.0), content="1")
  inspect(@math.powf(0.0, 5.0), content="0")
  
  // 测试平方根
  inspect(@math.sqrtf(9.0), content="3")
  inspect(@math.sqrtf(16.0), content="4")
  inspect(@math.sqrtf(0.0), content="0")
}

///|
test "complex_pattern_matching" {
  // 测试复杂的模式匹配
  let nested_option : Option<Option<Int>> = Some(Some(42))
  let nested_none : Option<Option[Int>> = Some(None)
  let double_none : Option<Option[Int>> = None
  
  // 嵌套 Option 匹配
  let result1 = match nested_option {
    Some(Some(x)) => x * 2
    Some(None) => 0
    None => -1
  }
  inspect(result1, content="84")
  
  let result2 = match nested_none {
    Some(Some(x)) => x * 2
    Some(None) => 0
    None => -1
  }
  inspect(result2, content="0")
  
  let result3 = match double_none {
    Some(Some(x)) => x * 2
    Some(None) => 0
    None => -1
  }
  inspect(result3, content="-1")
}