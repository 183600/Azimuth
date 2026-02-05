// 增强标准 MoonBit 测试用例 - 为 Azimuth 项目
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-4, add(-7, 3))
  assert_eq(200, add(100, 100))
  assert_eq(-50, add(-25, -25))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(7, 0))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(5, divide_with_ceil(14, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(8, 0))
}

test "divide_with_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-4, divide_with_ceil(-14, 3))
  assert_eq(-5, divide_with_ceil(14, -3))
  assert_eq(5, divide_with_ceil(-14, -3))
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(1, divide_with_ceil(-2, -3))
}

test "greet_function_unicode_support" {
  // 问候函数 Unicode 支持测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, café!", greet("café"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(4, subtract(7, 3))
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(10, 10))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(23, add(15, 8))
  assert_eq(120, multiply(15, 8))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let total_products = 127
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(8, remaining_space)
}