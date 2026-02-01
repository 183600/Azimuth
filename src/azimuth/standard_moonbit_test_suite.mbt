// 标准 MoonBit 测试套件 - azimuth 库
// 包含8个简洁的测试用例，覆盖库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(7, 3))
  assert_eq(-2, add(5, -7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-4, 3))
  assert_eq(4, divide_with_ceil(13, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
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

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))   // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0的安全处理
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_efficiency" {
  // 包装效率计算
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "complex_expression_evaluation" {
  // 复杂表达式求值
  let x = 10
  let y = 20
  let z = 5
  
  // (x + y) * z - x
  let result = add(multiply(add(x, y), z), -x)
  assert_eq(145, result)  // (10 + 20) * 5 - 10 = 150 - 10 = 140
  
  // 嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 3)
  assert_eq(67, nested)  // ceil((10 + 100) * 2 / 3) = ceil(220/3) = 74
}

test "error_handling_robustness" {
  // 错误处理健壮性
  // 除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(5, 0))
}