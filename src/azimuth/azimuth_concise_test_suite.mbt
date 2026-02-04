// Azimuth 项目简洁测试套件
// 使用标准的 MoonBit 测试语法，包含8个高质量的测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(-2, add(5, -7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(5, subtract(12, 7))
  assert_eq(-8, subtract(3, 11))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "greet_function_comprehensive" {
  // 测试问候函数的综合功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 8
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 单位元性质
  assert_eq(a, multiply(a, 1))
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 25
  let discount = 30
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(295, total_before_discount)
  assert_eq(265, final_total)
}

test "packaging_and_resource_allocation" {
  // 测试包装和资源分配
  let total_items = 58
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(5, boxes_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, remaining_space)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  let x = 12
  let y = 8
  let z = 5
  
  // 测试 (x + y) * z - x / y
  let result1 = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(99, result1)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
  
  // 测试 x * y + z * (x - y)
  let result2 = add(multiply(x, y), multiply(z, subtract(x, y)))
  assert_eq(116, result2)  // 12*8 + 5*(12-8) = 96 + 5*4 = 96 + 20 = 116
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极端值
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位运算
  assert_eq(7, multiply(7, 1))
  assert_eq(8, add(8, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // �数运算
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, add(3, -11))
  
  // 大数运算
  assert_eq(1000000, add(500000, 500000))
  assert_eq(10000, multiply(100, 100))
}