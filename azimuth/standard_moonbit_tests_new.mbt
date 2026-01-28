// 标准 MoonBit 测试用例 - 为 azimuth 库新增的测试
// 包含 8 个高质量测试用例，覆盖核心功能和边界情况

test "arithmetic_basic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-4, @azimuth.subtract(2, 6))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数处理
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 10
  let step1 = @azimuth.add(base, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)  // 30
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 4
  let result = @azimuth.subtract(step3, 1)  // 3
  
  @azimuth.assert_eq(3, result)
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(2, @azimuth.subtract(-1, -3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "boundary_value_operations" {
  // 边界值运算测试
  // 零值运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  
  // 单位值运算
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
}

test "real_world_business_scenario" {
  // 真实业务场景测试：装盒问题
  let items = 47
  let box_capacity = 12
  
  // 计算需要的盒子数量
  let boxes_needed = @azimuth.divide_with_ceil(items, box_capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(1, remaining_space)
}