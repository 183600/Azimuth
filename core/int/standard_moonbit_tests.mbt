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
test "int boundary values" {
  // 测试整数的边界值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试边界值的运算
  inspect(@int.max_value - 1, content="2147483646")
  inspect(@int.min_value + 1, content="-2147483647")
}

///|
test "int absolute value function" {
  // 测试绝对值函数
  inspect(@int.abs(0), content="0")
  inspect(@int.abs(5), content="5")
  inspect(@int.abs(-5), content="5")
  inspect(@int.abs(2147483647), content="2147483647")
  inspect(@int.abs(-2147483647), content="2147483647")
  // 注意：-2147483648 的绝对值会溢出
}

///|
test "int byte conversion" {
  // 测试整数到字节的转换
  inspect(0.to_byte(), content="0")
  inspect(255.to_byte(), content="255")
  inspect(-1.reinterpret_as_uint().to_byte(), content="255")
  
  // 测试大端字节序
  let big_bytes = 0x12345678.to_be_bytes()
  inspect(big_bytes.length(), content="4")
  
  // 测试小端字节序
  let little_bytes = 0x12345678.to_le_bytes()
  inspect(little_bytes.length(), content="4")
}