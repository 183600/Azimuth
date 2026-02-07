// 标准 Azimuth 增强测试套件
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "enhanced_addition_functionality" {
  // 增强加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(4, 6))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(200, @azimuth.add(100, 100))
}

test "enhanced_multiplication_functionality" {
  // 增强乘法功能测试
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "enhanced_divide_with_ceil_positive" {
  // 增强向上取整除法正数测试
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(6, 0))
}

test "enhanced_divide_with_ceil_negative" {
  // 增强向上取整除法负数测试
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(12, -3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-12, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "enhanced_greet_function_cases" {
  // 增强问候函数测试
  @azimuth.assert_eq_string("Hello, Universe!", @azimuth.greet("Universe"))
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
}

test "enhanced_subtract_function_cases" {
  // 增强减法函数测试
  @azimuth.assert_eq(3, @azimuth.subtract(8, 5))
  @azimuth.assert_eq(-3, @azimuth.subtract(5, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(5, -10))
}

test "enhanced_mathematical_properties" {
  // 增强数学性质测试
  let x = 9
  let y = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
}

test "enhanced_business_scenario" {
  // 增强业务场景测试
  let products = 48
  let box_capacity = 10
  let boxes_needed = @azimuth.divide_with_ceil(products, box_capacity)
  
  @azimuth.assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let unused_space = @azimuth.subtract(total_capacity, products)
  @azimuth.assert_eq(2, unused_space)
}