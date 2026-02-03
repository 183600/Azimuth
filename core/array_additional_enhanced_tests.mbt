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
test "array advanced filtering" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Filter multiples of 3
  let multiples_of_3 = numbers.filter(x => x % 3 == 0)
  inspect(multiples_of_3, content="[3, 6, 9]")
  
  // Filter prime numbers (simple check)
  let primes = numbers.filter(x => {
    if (x <= 1) { false }
    else if (x == 2) { true }
    else if (x % 2 == 0) { false }
    else {
      let mut is_prime = true
      for i = 3; i < x; i = i + 2 {
        if (x % i == 0) {
          is_prime = false
          break
        }
      }
      is_prime
    }
  })
  inspect(primes, content="[2, 3, 5, 7]")
}

///|
test "array complex mapping" {
  let words = ["hello", "world", "moonbit", "test"]
  
  // Map to lengths
  let lengths = words.map(w => w.length())
  inspect(lengths, content="[5, 5, 7, 4]")
  
  // Map to uppercase
  let uppercased = words.map(w => w.to_upper())
  inspect(uppercased, content="['HELLO', 'WORLD', 'MOONBIT', 'TEST']")
  
  // Map to first character
  let first_chars = words.map(w => w[0].to_string())
  inspect(first_chars, content="['h', 'w', 'm', 't']")
}

///|
test "array reduction patterns" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Sum with fold
  let sum = numbers.fold(0, (acc, x) => acc + x)
  inspect(sum, content="15")
  
  // Product with fold
  let product = numbers.fold(1, (acc, x) => acc * x)
  inspect(product, content="120")
  
  // Maximum with fold
  let maximum = numbers.fold(numbers[0], (acc, x) => if x > acc { x } else { acc })
  inspect(maximum, content="5")
  
  // Minimum with fold
  let minimum = numbers.fold(numbers[0], (acc, x) => if x < acc { x } else { acc })
  inspect(minimum, content="1")
}

///|
test "array transformation patterns" {
  let numbers = [1, 2, 3, 4, 5]
  
  // Square then filter even
  let squares_even = numbers.map(x => x * x).filter(x => x % 2 == 0)
  inspect(squares_even, content="[4, 16]")
  
  // Double then sum
  let double_sum = numbers.map(x => x * 2).fold(0, (acc, x) => acc + x)
  inspect(double_sum, content="30")
  
  // Filter odd then double
  let odd_doubled = numbers.filter(x => x % 2 != 0).map(x => x * 2)
  inspect(odd_doubled, content="[2, 6, 10]")
}

///|
test "array nested operations" {
  let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  
  // Flatten matrix
  let flattened = matrix.fold([], (acc, row) => acc + row)
  inspect(flattened, content="[1, 2, 3, 4, 5, 6, 7, 8, 9]")
  
  // Sum all elements
  let total_sum = flattened.fold(0, (acc, x) => acc + x)
  inspect(total_sum, content="45")
  
  // Sum each row
  let row_sums = matrix.map(row => row.fold(0, (acc, x) => acc + x))
  inspect(row_sums, content="[6, 15, 24]")
}

///|
test "array string operations" {
  let words = ["hello", "world", "moonbit"]
  
  // Join with spaces
  let joined = words.fold("", (acc, word) => 
    if acc == "" { word } else { acc + " " + word }
  )
  inspect(joined, content="hello world moonbit")
  
  // Create acronym
  let acronym = words.map(w => w[0].to_string()).fold("", (acc, char) => acc + char)
  inspect(acronym, content="hwm")
  
  // Count total characters
  let total_chars = words.fold(0, (acc, word) => acc + word.length())
  inspect(total_chars, content="17")
}

///|
test "array search and find patterns" {
  let numbers = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  
  // Find first multiple of 7
  let multiple_of_7 = numbers.find(x => x % 7 == 0)
  inspect(multiple_of_7, content="Some(70)")
  
  // Find first greater than 95
  let greater_than_95 = numbers.find(x => x > 95)
  inspect(greater_than_95, content="Some(100)")
  
  // Find first negative (should be None)
  let negative = numbers.find(x => x < 0)
  inspect(negative, content="None")
  
  // Check if all are positive
  let all_positive = numbers.fold(true, (acc, x) => acc && (x > 0))
  inspect(all_positive, content="true")
  
  // Check if any are divisible by 3
  let any_divisible_by_3 = numbers.fold(false, (acc, x) => acc || (x % 3 == 0))
  inspect(any_divisible_by_3, content="true")
}

///|
test "array boundary and edge cases" {
  let empty : Array[Int] = []
  let single = [42]
  let large = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Empty array operations
  inspect(empty.length(), content="0")
  inspect(empty.fold(0, (acc, x) => acc + x), content="0")
  inspect(empty.filter(x => x > 0), content="[]")
  inspect(empty.map(x => x * 2), content="[]")
  
  // Single element operations
  inspect(single.length(), content="1")
  inspect(single[0], content="42")
  inspect(single.fold(0, (acc, x) => acc + x), content="42")
  inspect(single.map(x => x * 2), content="[84]")
  
  // Large array operations
  inspect(large.length(), content="10")
  inspect(large.fold(0, (acc, x) => acc + x), content="55")
}