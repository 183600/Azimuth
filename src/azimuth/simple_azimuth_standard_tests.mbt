// Azimuth 项目标准 MoonBit 测试用例
// 使用自定义函数测试核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiplication_operations" {
  // 测试乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "subtraction_operations" {
  // 测试减法运算
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "division_operations" {
  // 测试除法运算
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_calculations" {
  // 测试复合计算
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = subtract(step2, 8)     // 22
  let step4 = divide_with_ceil(step3, 4)  // 6
  
  assert_eq(step1, 15)
  assert_eq(step2, 30)
  assert_eq(step3, 22)
  assert_eq(step4, 6)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
  
  // 减法的逆运算
  assert_eq(add(subtract(a, b), b), a)
}

test "boundary_values" {
  // 测试边界值
  // 零值边界
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 单位值边界
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 100), 100)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(100, 1), 100)
  
  // 负单位边界
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 100), -100)
  assert_eq(subtract(0, -1), 1)
  assert_eq(divide_with_ceil(-100, 1), -100)
}

test "shopping_cart_simulation" {
  // 测试购物车计算模拟
  // 商品价格和数量
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let item1_total = multiply(item1_price, item1_quantity)  // 75
  let item2_total = multiply(item2_price, item2_quantity)  // 30
  
  // 计算总计
  let subtotal = add(item1_total, item2_total)  // 105
  
  // 计算税费（10%，取整数部分）
  let tax = divide_with_ceil(multiply(subtotal, 10), 100)  // 11
  
  // 计算总价
  let total = add(subtotal, tax)  // 116
  
  assert_eq(item1_total, 75)
  assert_eq(item2_total, 30)
  assert_eq(subtotal, 105)
  assert_eq(tax, 11)
  assert_eq(total, 116)
}

test "error_handling_simulation" {
  // 测试错误处理模拟
  // 大数运算
  let large_num = 100000
  let large_sum = add(large_num, large_num)
  let large_product = multiply(large_num, 2)
  
  assert_eq(large_sum, 200000)
  assert_eq(large_product, 200000)
  
  // 除零错误处理
  let result1 = divide_with_ceil(50, 0)
  let result2 = divide_with_ceil(0, 0)
  let result3 = divide_with_ceil(-50, 0)
  
  // 验证错误情况下的默认行为
  assert_eq(result1, 0)
  assert_eq(result2, 0)
  assert_eq(result3, 0)
}