// 标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "addition_comprehensive" {
  // 加法运算综合测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-5, 5))
}

test "multiplication_comprehensive" {
  // 乘法运算综合测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
  assert_eq(-10000, multiply(-100, 100))
}

test "subtraction_comprehensive" {
  // 减法运算综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(0, subtract(-5, -5))
}

test "division_ceil_positive" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(2, divide_with_ceil(6, 3))
}

test "division_ceil_negative" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "greet_function_various" {
  // 问候函数多种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_properties" {
  // 数学性质验证测试
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
}

test "boundary_values" {
  // 边界值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, subtract(1, 0))
  assert_eq(10, divide_with_ceil(10, 1))
}

test "complex_calculations" {
  // 复杂计算测试
  let x = 5
  let y = 3
  let z = 2
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(16, result1)
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(17, result2)
  
  // (x - y) / z (向上取整)
  let result3 = divide_with_ceil(subtract(x, y), z)
  assert_eq(1, result3)
}

test "real_world_scenarios" {
  // 真实世界场景测试
  // 购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  let subtotal = multiply(item_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(100, subtotal)
  assert_eq(10, tax)
  assert_eq(110, total)
  
  // 包装计算
  let items = 25
  let package_capacity = 6
  let packages_needed = divide_with_ceil(items, package_capacity)
  
  assert_eq(5, packages_needed)
}