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
test "int max_value and min_value" {
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
}

///|
test "int abs function" {
  inspect(@int.abs(5), content="5")
  inspect(@int.abs(-5), content="5")
  inspect(@int.abs(0), content="0")
  inspect(@int.abs(@int.min_value), content="-2147483648") // Note: abs(min_value) overflows
}

///|
test "int to_be_bytes" {
  let bytes = @int.to_be_bytes(0x12345678)
  inspect(bytes.length, content="4")
  inspect(bytes[0], content="18")
  inspect(bytes[1], content="52")
  inspect(bytes[2], content="86")
  inspect(bytes[3], content="120")
}

///|
test "int to_le_bytes" {
  let bytes = @int.to_le_bytes(0x12345678)
  inspect(bytes.length, content="4")
  inspect(bytes[0], content="120")
  inspect(bytes[1], content="86")
  inspect(bytes[2], content="52")
  inspect(bytes[3], content="18")
}

///|
test "int to_be_bytes with negative numbers" {
  let bytes = @int.to_be_bytes(-1)
  inspect(bytes.length, content="4")
  inspect(bytes[0], content="255")
  inspect(bytes[1], content="255")
  inspect(bytes[2], content="255")
  inspect(bytes[3], content="255")
}

///|
test "int to_le_bytes with negative numbers" {
  let bytes = @int.to_le_bytes(-1)
  inspect(bytes.length, content="4")
  inspect(bytes[0], content="255")
  inspect(bytes[1], content="255")
  inspect(bytes[2], content="255")
  inspect(bytes[3], content="255")
}