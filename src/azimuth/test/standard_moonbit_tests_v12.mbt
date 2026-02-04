// 标准 MoonBit 测试用例
// 测试 azimuth 项目的基本功能

// 测试 add 函数的基本功能
test "add_basic_functionality" {
  let result1 = @azimuth.add(1, 2)
  if (result1 != 3) {
    @builtin.abort("@azimuth.add(1, 2) should be 3")
  }
  
  let result2 = @azimuth.add(-1, 1)
  if (result2 != 0) {
    @builtin.abort("@azimuth.add(-1, 1) should be 0")
  }
  
  let result3 = @azimuth.add(0, 5)
  if (result3 != 5) {
    @builtin.abort("@azimuth.add(0, 5) should be 5")
  }
}

// 测试 add 函数的溢出处理
test "add_overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = @azimuth.add(max_val, 1)
  if (result1 != max_val) {
    @builtin.abort("@azimuth.add(max_val, 1) should return max_val")
  }
  
  // 测试负数溢出
  let result2 = @azimuth.add(min_val, -1)
  if (result2 != min_val) {
    @builtin.abort("@azimuth.add(min_val, -1) should return min_val")
  }
  
  // 测试最大值与最小值相加
  let result3 = @azimuth.add(max_val, min_val)
  if (result3 != -1) {
    @builtin.abort("@azimuth.add(max_val, min_val) should be -1")
  }
}

// 测试 multiply 函数的基本功能
test "multiply_basic_functionality" {
  let result1 = @azimuth.multiply(3, 4)
  if (result1 != 12) {
    @builtin.abort("@azimuth.multiply(3, 4) should be 12")
  }
  
  let result2 = @azimuth.multiply(-2, 3)
  if (result2 != -6) {
    @builtin.abort("@azimuth.multiply(-2, 3) should be -6")
  }
  
  let result3 = @azimuth.multiply(0, 100)
  if (result3 != 0) {
    @builtin.abort("@azimuth.multiply(0, 100) should be 0")
  }
}

// 测试 multiply 函数的边界情况
test "multiply_boundary_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1相乘
  let result1 = @azimuth.multiply(max_val, 1)
  if (result1 != max_val) {
    @builtin.abort("@azimuth.multiply(max_val, 1) should return max_val")
  }
  
  // 测试与-1相乘
  let result2 = @azimuth.multiply(max_val, -1)
  if (result2 != min_val) {
    @builtin.abort("@azimuth.multiply(max_val, -1) should return min_val")
  }
  
  // 测试最小值与-1相乘的特殊情况
  let result3 = @azimuth.multiply(min_val, -1)
  if (result3 != min_val) {
    @builtin.abort("@azimuth.multiply(min_val, -1) should return min_val")
  }
}

// 测试 greet 函数的基本功能
test "greet_basic_functionality" {
  let result1 = @azimuth.greet("World")
  if (result1 != "Hello, World!") {
    @builtin.abort("@azimuth.greet(\"World\") should be \"Hello, World!\"")
  }
  
  let result2 = @azimuth.greet("")
  if (result2 != "Hello, !") {
    @builtin.abort("@azimuth.greet(\"\") should be \"Hello, !\"")
  }
  
  let result3 = @azimuth.greet("测试")
  if (result3 != "Hello, 测试!") {
    @builtin.abort("@azimuth.greet(\"测试\") should be \"Hello, 测试!\"")
  }
}

// 测试 greet 函数的特殊字符处理
test "greet_special_characters" {
  let result1 = @azimuth.greet("John@123")
  if (result1 != "Hello, John@123!") {
    @builtin.abort("greet should handle special characters correctly")
  }
  
  let result2 = @azimuth.greet("   ")
  if (result2 != "Hello,    !") {
    @builtin.abort("greet should handle whitespace correctly")
  }
  
  let result3 = @azimuth.greet("🌍")
  if (result3 != "Hello, 🌍!") {
    @builtin.abort("greet should handle emoji correctly")
  }
}

// 测试复合运算场景
test "complex_calculation_scenario" {
  // 测试 (a + b) * c 的计算
  let a = 5
  let b = 3
  let c = 2
  
  let sum = @azimuth.add(a, b)
  if (sum != 8) {
    @builtin.abort("@azimuth.add(5, 3) should be 8")
  }
  
  let product = @azimuth.multiply(sum, c)
  if (product != 16) {
    @builtin.abort("@azimuth.multiply(8, 2) should be 16")
  }
}

// 测试实际应用场景：购物车计算
test "shopping_cart_calculation" {
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  // 验证总价：100*2 + 50*3 = 200 + 150 = 350
  if (cart_total != 350) {
    @builtin.abort("Shopping cart total should be 350")
  }
}

// 测试数学交换律
test "mathematical_commutativity" {
  // 测试加法交换律
  let a = 15
  let b = 25
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  if (result1 != result2) {
    @builtin.abort("Addition should be commutative")
  }
  
  // 测试乘法交换律
  let c = 6
  let d = 7
  
  let result3 = @azimuth.multiply(c, d)
  let result4 = @azimuth.multiply(d, c)
  if (result3 != result4) {
    @builtin.abort("Multiplication should be commutative")
  }
}

// 测试边界值组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与0的运算
  let result1 = @azimuth.add(max_val, 0)
  if (result1 != max_val) {
    @builtin.abort("@azimuth.add(max_val, 0) should return max_val")
  }
  
  let result2 = @azimuth.multiply(max_val, 0)
  if (result2 != 0) {
    @builtin.abort("@azimuth.multiply(max_val, 0) should return 0")
  }
  
  // 测试最小值与0的运算
  let result3 = @azimuth.add(min_val, 0)
  if (result3 != min_val) {
    @builtin.abort("@azimuth.add(min_val, 0) should return min_val")
  }
  
  let result4 = @azimuth.multiply(min_val, 0)
  if (result4 != 0) {
    @builtin.abort("@azimuth.multiply(min_val, 0) should return 0")
  }
}