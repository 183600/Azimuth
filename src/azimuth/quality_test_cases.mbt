// 高质量 MoonBit 测试用例
// 为 Azimuth 项目添加的标准测试

test "arithmetic_basic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-5, 2))
  assert_eq(200, add(100, 100))
}

test "multiplication_comprehensive" {
  // 综合乘法测试
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(12, 0))
  assert_eq(-45, multiply(-5, 9))
  assert_eq(64, multiply(-8, -8))
}

test "division_ceil_positive" {
  // 正数向上取整除法测试
  assert_eq(5, divide_with_ceil(14, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "division_ceil_negative" {
  // 负数向上取整除法测试
  assert_eq(-4, divide_with_ceil(-14, 3))
  assert_eq(-5, divide_with_ceil(14, -3))
  assert_eq(5, divide_with_ceil(-14, -3))
  assert_eq(-2, divide_with_ceil(-3, 2))
}

test "greeting_functionality" {
  // 问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 2024!", greet("2024"))
}

test "subtraction_operations" {
  // 减法运算测试
  assert_eq(6, subtract(15, 9))
  assert_eq(-9, subtract(6, 15))
  assert_eq(0, subtract(20, 20))
  assert_eq(25, subtract(10, -15))
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 16
  let y = 24
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(40, add(16, 24))
  assert_eq(384, multiply(16, 24))
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let products = 85
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(products, box_capacity)
  
  assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, products)
  assert_eq(11, remaining_space)
}