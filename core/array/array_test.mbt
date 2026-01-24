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
test "zip_with" {
  let left = [1, 2, 3]
  let right = [10, 20]
  inspect(@array.zip_with(left, right, (a, b) => a + b), content="[11, 22]")
}

///|
test "array basic operations enhanced" {
  let empty = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c"]
  inspect(empty.length(), content="0")
  inspect(numbers.length(), content="5")
  inspect(strings.length(), content="3")
  inspect(numbers[0], content="1")
  inspect(strings[2], content="c")
}

///|
test "array zip_with operation enhanced" {
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let sum = (a, b) => a + b
  let result = @array.zip_with(arr1, arr2, sum)?
  inspect(result, content="[5, 7, 9]")
}
