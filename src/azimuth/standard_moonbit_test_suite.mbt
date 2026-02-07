// 标准 MoonBit 测试套件
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition_comprehensive" {
  // 全面的加法运算测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(1, add(-2, 3))
  assert_eq(-5, add(-3, -2))
}

test "basic_arithmetic_multiplication_comprehensive" {
  // 全面的乘法运算测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1, multiply(1, 1))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(2, divide_with_ceil(5, 3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "subtract_function_comprehensive" {
  // 全面的减法函数测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(15, subtract(-5, -20))
  assert_eq(-15, subtract(5, 20))
}

test "greet_function_edge_cases" {
  // 问候函数边界情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 单位元性质
  assert_eq(add(a, 0), a)
  assert_eq(multiply(a, 1), a)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let x = 10
  let y = 3
  let z = 2
  
  // 复合运算测试
  let result1 = add(multiply(x, y), z)  // 10 * 3 + 2 = 32
  let result2 = multiply(add(x, y), z)  // (10 + 3) * 2 = 26
  let result3 = subtract(multiply(x, y), z)  // 10 * 3 - 2 = 28
  let result4 = divide_with_ceil(add(x, y), z)  // ceil((10 + 3) / 2) = 7
  
  assert_eq(result1, 32)
  assert_eq(result2, 26)
  assert_eq(result3, 28)
  assert_eq(result4, 7)
}

test "boundary_value_analysis" {
  // 边界值分析测试
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
  
  // 大数边界
  let large_num = 100000
  assert_eq(add(large_num, large_num), 200000)
  assert_eq(divide_with_ceil(large_num, 3), 33334)
}

test "real_world_application_test" {
  // 真实世界应用测试
  // 购物车计算场景
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 10
  
  // 计算商品小计
  let subtotal1 = multiply(item1_price, item1_qty)  // 25 * 3 = 75
  let subtotal2 = multiply(item2_price, item2_qty)  // 15 * 2 = 30
  let total_subtotal = add(subtotal1, subtotal2)    // 75 + 30 = 105
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(total_subtotal, tax_rate), 100)  // ceil(105 * 10 / 100) = 11
  
  // 计算总价
  let grand_total = add(total_subtotal, tax_amount)  // 105 + 11 = 116
  
  // 验证计算结果
  assert_eq(subtotal1, 75)
  assert_eq(subtotal2, 30)
  assert_eq(total_subtotal, 105)
  assert_eq(tax_amount, 11)
  assert_eq(grand_total, 116)
  
  // 测试客户问候
  let customer_name = "张三"
  let greeting = greet(customer_name)
  assert_eq_string("Hello, 张三!", greeting)
}