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

// 标准 MoonBit 核心库测试用例
// 包含 10 个高质量测试用例，覆盖核心功能

///|
test "int_boundary_values" {
  // 测试整数边界值
  inspect(Int::max_value, content="2147483647")
  inspect(Int::min_value, content="-2147483648")
  inspect(Int::abs(-42), content="42")
  inspect(Int::abs(0), content="0")
  inspect(Int::abs(42), content="42")
}

///|
test "bool_conversions" {
  // 测试布尔类型转换
  inspect(true.to_int(), content="1")
  inspect(false.to_int(), content="0")
  inspect(true.to_string(), content="true")
  inspect(false.to_string(), content="false")
  inspect((1 == 1), content="true")
  inspect((1 == 2), content="false")
}

///|
test "string_basic_operations" {
  // 测试字符串基本操作
  inspect(String::default(), content="")
  inspect("Hello" + " " + "World", content="Hello World")
  inspect(String::length(""), content="0")
  inspect(String::length("MoonBit"), content="7")
  inspect(String::length("测试"), content="2")
}

///|
test "mathematical_operations" {
  // 测试数学运算
  inspect(1 + 2 * 3, content="7")
  inspect((1 + 2) * 3, content="9")
  inspect(10 / 3, content="3")
  inspect(10 % 3, content="1")
  inspect(-5 * -2, content="10")
}

///|
test "option_type_operations" {
  // 测试 Option 类型操作
  inspect(Option::some(42), content="Some(42)")
  inspect(Option::none(), content="None")
  inspect(Option::some("test"), content="Some(test)")
  inspect(Option::is_some(Option::some(42)), content="true")
  inspect(Option::is_some(Option::none()), content="false")
}

///|
test "result_type_operations" {
  // 测试 Result 类型操作
  inspect(Result::ok(42), content="Ok(42)")
  inspect(Result::error("failed"), content="Error(failed)")
  inspect(Result::is_ok(Result::ok(42)), content="true")
  inspect(Result::is_ok(Result::error("failed")), content="false")
  inspect(Result::is_error(Result::error("failed")), content="true")
}

///|
test "comparison_operations" {
  // 测试比较操作
  inspect(1 < 2, content="true")
  inspect(2 < 1, content="false")
  inspect(1 <= 1, content="true")
  inspect(1 >= 1, content="true")
  inspect(1 > 2, content="false")
  inspect(2 > 1, content="true")
  inspect(1 == 1, content="true")
  inspect(1 == 2, content="false")
  inspect(1 != 2, content="true")
  inspect(1 != 1, content="false")
}

///|
test "unicode_string_handling" {
  // 测试 Unicode 字符串处理
  inspect("Hello, 世界", content="Hello, 世界")
  inspect("🚀 MoonBit 🌙", content="🚀 MoonBit 🌙")
  inspect(String::length("世界"), content="2")
  inspect(String::length("🚀"), content="1")
  inspect("café", content="café")
  inspect("Москва", content="Москва")
}

///|
test "type_conversions" {
  // 测试类型转换
  inspect(42.to_string(), content="42")
  inspect((-42).to_string(), content="-42")
  inspect(0.to_string(), content="0")
  inspect(2147483647.to_string(), content="2147483647")
  inspect((-2147483648).to_string(), content="-2147483648")
  inspect(true.to_int(), content="1")
  inspect(false.to_int(), content="0")
}

///|
test "logical_operations" {
  // 测试逻辑运算
  inspect(true && true, content="true")
  inspect(true && false, content="false")
  inspect(false && true, content="false")
  inspect(false && false, content="false")
  inspect(true || true, content="true")
  inspect(true || false, content="true")
  inspect(false || true, content="true")
  inspect(false || false, content="false")
  inspect(!true, content="false")
  inspect(!false, content="true")
}