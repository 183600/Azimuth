// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  let result1 = add(2, 3)
  if (result1 != 5) {
    @builtin.abort("Test failed: add(2, 3) should be 5")
  }
  
  let result2 = add(0, 0)
  if (result2 != 0) {
    @builtin.abort("Test failed: add(0, 0) should be 0")
  }
  
  let result3 = add(-2, 3)
  if (result3 != 1) {
    @builtin.abort("Test failed: add(-2, 3) should be 1")
  }
  
  let result4 = add(100, 200)
  if (result4 != 300) {
    @builtin.abort("Test failed: add(100, 200) should be 300")
  }
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  let result1 = multiply(3, 4)
  if (result1 != 12) {
    @builtin.abort("Test failed: multiply(3, 4) should be 12")
  }
  
  let result2 = multiply(0, 5)
  if (result2 != 0) {
    @builtin.abort("Test failed: multiply(0, 5) should be 0")
  }
  
  let result3 = multiply(-2, 3)
  if (result3 != -6) {
    @builtin.abort("Test failed: multiply(-2, 3) should be -6")
  }
  
  let result4 = multiply(-2, -3)
  if (result4 != 6) {
    @builtin.abort("Test failed: multiply(-2, -3) should be 6")
  }
}

test "subtract_function_basic" {
  // 测试基本减法功能
  let result1 = subtract(10, 3)
  if (result1 != 7) {
    @builtin.abort("Test failed: subtract(10, 3) should be 7")
  }
  
  let result2 = subtract(3, 10)
  if (result2 != -7) {
    @builtin.abort("Test failed: subtract(3, 10) should be -7")
  }
  
  let result3 = subtract(0, 0)
  if (result3 != 0) {
    @builtin.abort("Test failed: subtract(0, 0) should be 0")
  }
  
  let result4 = subtract(-5, -3)
  if (result4 != -2) {
    @builtin.abort("Test failed: subtract(-5, -3) should be -2")
  }
}

test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  let result1 = divide_with_ceil(10, 3)
  if (result1 != 4) {
    @builtin.abort("Test failed: divide_with_ceil(10, 3) should be 4")
  }
  
  let result2 = divide_with_ceil(9, 3)
  if (result2 != 3) {
    @builtin.abort("Test failed: divide_with_ceil(9, 3) should be 3")
  }
  
  let result3 = divide_with_ceil(0, 5)
  if (result3 != 0) {
    @builtin.abort("Test failed: divide_with_ceil(0, 5) should be 0")
  }
  
  let result4 = divide_with_ceil(5, 0)
  if (result4 != 0) {
    @builtin.abort("Test failed: divide_with_ceil(5, 0) should be 0")
  }
  
  let result5 = divide_with_ceil(-10, 3)
  if (result5 != -3) {
    @builtin.abort("Test failed: divide_with_ceil(-10, 3) should be -3")
  }
}

test "greet_function_basic" {
  // 测试问候函数功能
  let result1 = greet("World")
  if (result1 != "Hello, World!") {
    @builtin.abort("Test failed: greet(\"World\") should be \"Hello, World!\"")
  }
  
  let result2 = greet("MoonBit")
  if (result2 != "Hello, MoonBit!") {
    @builtin.abort("Test failed: greet(\"MoonBit\") should be \"Hello, MoonBit!\"")
  }
  
  let result3 = greet("")
  if (result3 != "Hello, !") {
    @builtin.abort("Test failed: greet(\"\") should be \"Hello, !\"")
  }
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  if (result1 != 30) {
    @builtin.abort("Test failed: (10 + 5) * 2 should be 30")
  }
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  if (result2 != 52) {
    @builtin.abort("Test failed: 10 * 5 + 2 should be 52")
  }
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  let add1 = add(x, y)
  let add2 = add(y, x)
  if (add1 != add2) {
    @builtin.abort("Test failed: add(x, y) should equal add(y, x)")
  }
  
  // 乘法交换律
  let mul1 = multiply(x, y)
  let mul2 = multiply(y, x)
  if (mul1 != mul2) {
    @builtin.abort("Test failed: multiply(x, y) should equal multiply(y, x)")
  }
  
  // 加法单位元
  let add_id = add(x, 0)
  if (add_id != x) {
    @builtin.abort("Test failed: add(x, 0) should equal x")
  }
  
  // 乘法单位元
  let mul_id = multiply(x, 1)
  if (mul_id != x) {
    @builtin.abort("Test failed: multiply(x, 1) should equal x")
  }
}

test "boundary_values" {
  // 测试边界值
  let result1 = add(0, 0)
  if (result1 != 0) {
    @builtin.abort("Test failed: add(0, 0) should be 0")
  }
  
  let result2 = multiply(0, 1)
  if (result2 != 0) {
    @builtin.abort("Test failed: multiply(0, 1) should be 0")
  }
  
  let result3 = subtract(0, 0)
  if (result3 != 0) {
    @builtin.abort("Test failed: subtract(0, 0) should be 0")
  }
  
  let result4 = divide_with_ceil(0, 1)
  if (result4 != 0) {
    @builtin.abort("Test failed: divide_with_ceil(0, 1) should be 0")
  }
  
  let result5 = divide_with_ceil(1, 1)
  if (result5 != 1) {
    @builtin.abort("Test failed: divide_with_ceil(1, 1) should be 1")
  }
}

test "negative_number_operations" {
  // 测试负数运算
  let result1 = add(-5, -3)
  if (result1 != -8) {
    @builtin.abort("Test failed: add(-5, -3) should be -8")
  }
  
  let result2 = multiply(-4, 5)
  if (result2 != -20) {
    @builtin.abort("Test failed: multiply(-4, 5) should be -20")
  }
  
  let result3 = subtract(-10, 5)
  if (result3 != -15) {
    @builtin.abort("Test failed: subtract(-10, 5) should be -15")
  }
  
  let result4 = divide_with_ceil(-10, 3)
  if (result4 != -3) {
    @builtin.abort("Test failed: divide_with_ceil(-10, 3) should be -3")
  }
}