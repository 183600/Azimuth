// 标准 MoonBit 核心测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_various_scenarios" {
  // 测试减法函数的各种场景
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(100, 100))
  assert_eq(200, subtract(100, -100))
  assert_eq(-200, subtract(-100, 100))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 35
  let quantity = 6
  let shipping = 18
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(210, subtotal)
  assert_eq(228, total_before_discount)
  assert_eq(203, final_total)
}

test "packaging_optimization_test" {
  // 包装优化测试
  let total_items = 58
  let items_per_box = 15
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(2, remaining_space)
}

test "negative_number_division_ceil" {
  // 负数向上取整除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let x = 10
  let y = 20
  let z = 5
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(150, result1)
  
  // 测试 x * y + z * 2
  let result2 = add(multiply(x, y), multiply(z, 2))
  assert_eq(210, result2)
  
  // 测试组合运算
  let result3 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result3)
}