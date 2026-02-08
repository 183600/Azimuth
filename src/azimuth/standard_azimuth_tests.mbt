// 标准 Azimuth 测试用例
// 使用标准 MoonBit 测试语法编写
// 包含 10 个高质量测试用例

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "arithmetic_multiplication_comprehensive" {
  // 综合乘法测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "division_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "division_with_ceil_negative_numbers" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "subtraction_operations" {
  // 减法操作测试
  assert_eq(3, subtract(7, 4))
  assert_eq(-3, subtract(4, 7))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
}

test "greet_function_standard_cases" {
  // 问候函数标准情况测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, Developer!", greet("Developer"))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "greet_function_edge_cases" {
  // 问候函数边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 20
  let factor = 5
  let offset = 8
  let divisor = 3
  
  let step1 = multiply(initial, factor)
  let step2 = add(step1, offset)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(100, step1)
  assert_eq(108, step2)
  assert_eq(36, result)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "practical_business_calculation" {
  // 实际业务计算测试
  // 模拟订单计算：商品单价 × 数量 + 运费 - 折扣
  let unit_price = 35
  let quantity = 6
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let before_discount = add(subtotal, shipping)
  let total = subtract(before_discount, discount)
  
  // 计算每件商品的平均成本（向上取整）
  let avg_cost = divide_with_ceil(total, quantity)
  
  assert_eq(210, subtotal)
  assert_eq(225, before_discount)
  assert_eq(205, total)
  assert_eq(35, avg_cost)
}