// 增强的 MoonBit 测试用例
// 包含各种实际应用场景和边界条件测试

test "basic_arithmetic_test" {
  let result1 = 2 + 3
  let result2 = 4 * 5
  let result3 = 10 - 3
  let result4 = 20 / 4
  
  if (result1 != 5) {
    @builtin.abort("Addition test failed")
  }
  if (result2 != 20) {
    @builtin.abort("Multiplication test failed")
  }
  if (result3 != 7) {
    @builtin.abort("Subtraction test failed")
  }
  if (result4 != 5) {
    @builtin.abort("Division test failed")
  }
}

test "negative_numbers_test" {
  let result1 = -5 + 3
  let result2 = -4 * 3
  let result3 = -10 - (-5)
  let result4 = -15 / 3
  
  if (result1 != -2) {
    @builtin.abort("Negative addition test failed")
  }
  if (result2 != -12) {
    @builtin.abort("Negative multiplication test failed")
  }
  if (result3 != -5) {
    @builtin.abort("Negative subtraction test failed")
  }
  if (result4 != -5) {
    @builtin.abort("Negative division test failed")
  }
}

test "large_numbers_test" {
  let result1 = 1000000 + 2000000
  let result2 = 10000 * 10000
  let result3 = 1000000 - 500000
  
  if (result1 != 3000000) {
    @builtin.abort("Large numbers addition test failed")
  }
  if (result2 != 100000000) {
    @builtin.abort("Large numbers multiplication test failed")
  }
  if (result3 != 500000) {
    @builtin.abort("Large numbers subtraction test failed")
  }
}

test "compound_calculations_test" {
  let result1 = (2 + 3) * 4
  let result2 = 10 + 20 * 3
  let result3 = (100 - 50) / 2
  
  if (result1 != 20) {
    @builtin.abort("Compound calculation 1 test failed")
  }
  if (result2 != 70) {
    @builtin.abort("Compound calculation 2 test failed")
  }
  if (result3 != 25) {
    @builtin.abort("Compound calculation 3 test failed")
  }
}

test "boolean_logic_test" {
  let result1 = 5 > 3
  let result2 = 2 < 1
  let result3 = 5 == 5
  let result4 = 3 != 4
  
  if (result1 != true) {
    @builtin.abort("Boolean logic 1 test failed")
  }
  if (result2 != false) {
    @builtin.abort("Boolean logic 2 test failed")
  }
  if (result3 != true) {
    @builtin.abort("Boolean logic 3 test failed")
  }
  if (result4 != true) {
    @builtin.abort("Boolean logic 4 test failed")
  }
}

test "string_operations_test" {
  let result1 = "Hello" + " " + "World"
  let result2 = "Test".length()
  
  if (result1 != "Hello World") {
    @builtin.abort("String concatenation test failed")
  }
  if (result2 != 4) {
    @builtin.abort("String length test failed")
  }
}

test "modulo_operations_test" {
  let result1 = 10 % 3
  let result2 = 15 % 5
  let result3 = 20 % 7
  
  if (result1 != 1) {
    @builtin.abort("Modulo operation 1 test failed")
  }
  if (result2 != 0) {
    @builtin.abort("Modulo operation 2 test failed")
  }
  if (result3 != 6) {
    @builtin.abort("Modulo operation 3 test failed")
  }
}

test "order_of_operations_test" {
  let result1 = 2 + 3 * 4
  let result2 = (2 + 3) * 4
  let result3 = 10 - 6 / 2
  let result4 = (10 - 6) / 2
  
  if (result1 != 14) {
    @builtin.abort("Order of operations 1 test failed")
  }
  if (result2 != 20) {
    @builtin.abort("Order of operations 2 test failed")
  }
  if (result3 != 7) {
    @builtin.abort("Order of operations 3 test failed")
  }
  if (result4 != 2) {
    @builtin.abort("Order of operations 4 test failed")
  }
}

test "boundary_values_test" {
  let max_int = 2147483647
  let min_int = -2147483648
  
  let result1 = max_int - 1
  let result2 = min_int + 1
  let result3 = max_int % 2
  let result4 = min_int % 2
  
  if (result1 != 2147483646) {
    @builtin.abort("Boundary value 1 test failed")
  }
  if (result2 != -2147483647) {
    @builtin.abort("Boundary value 2 test failed")
  }
  if (result3 != 1) {
    @builtin.abort("Boundary value 3 test failed")
  }
  if (result4 != 0) {
    @builtin.abort("Boundary value 4 test failed")
  }
}

test "complex_expression_test" {
  let a = 5
  let b = 3
  let c = 2
  
  let result1 = a * b + c
  let result2 = a * (b + c)
  let result3 = (a + b) * c
  let result4 = a + b * c
  
  if (result1 != 17) {
    @builtin.abort("Complex expression 1 test failed")
  }
  if (result2 != 25) {
    @builtin.abort("Complex expression 2 test failed")
  }
  if (result3 != 16) {
    @builtin.abort("Complex expression 3 test failed")
  }
  if (result4 != 11) {
    @builtin.abort("Complex expression 4 test failed")
  }
}