// 标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖 azimuth 库的主要功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(-5, 10), @azimuth.multiply(10, -5))
}

test "zero_element_properties" {
  // 测试零元素性质
  // 加法零元素
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法零元素
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  // 负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
  
  // 负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 嵌套运算：(a + b) * c
  let sum = @azimuth.add(5, 3)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(16, product)
  
  // 连续运算链
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "boundary_value_operations" {
  // 测试边界值运算
  // 边界值加法
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  
  // 边界值乘法（不溢出的情况）
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
}

test "real_world_application_test" {
  // 测试真实世界应用场景
  // 场景：计算装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}