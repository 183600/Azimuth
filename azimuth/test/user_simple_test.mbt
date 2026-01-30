// 新增的标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含8个高质量测试用例，使用正确的 MoonBit 测试语法

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  @azimuth.assert_eq(7, @azimuth.subtract(10, 3))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
  @azimuth.assert_eq(-2, @azimuth.subtract(-5, -3))
  @azimuth.assert_eq(8, @azimuth.subtract(5, -3))
  @azimuth.assert_eq(-8, @azimuth.subtract(-5, 3))
  @azimuth.assert_eq(0, @azimuth.subtract(100, 100))
}

test "arithmetic_operation_chaining" {
  // 测试算术运算链式组合
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.subtract(10, 4))
  @azimuth.assert_eq(12, result1)  // 2*3 + (10-4) = 6 + 6 = 12
  
  let result2 = @azimuth.divide_with_ceil(
    @azimuth.subtract(@azimuth.multiply(5, 4), @azimuth.add(3, 2)),
    @azimuth.multiply(2, 1)
  )
  @azimuth.assert_eq(8, result2)  // ceil((20-5)/2) = ceil(15/2) = 8
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$%!", @azimuth.greet("!@#$%"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边缘情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
}

test "mathematical_properties" {
  // 测试数学恒等式性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(42, @azimuth.subtract(42, 0))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let step1 = @azimuth.multiply(8, 5)
  @azimuth.assert_eq(40, step1)
  
  let step2 = @azimuth.multiply(step1, 2)
  @azimuth.assert_eq(80, step2)
  
  let step3 = @azimuth.divide_with_ceil(150, step2)
  @azimuth.assert_eq(2, step3)
  
  let step4 = @azimuth.subtract(@azimuth.multiply(step3, step2), 150)
  @azimuth.assert_eq(10, step4)
}

test "business_calculation_scenario" {
  // 业务计算场景：库存管理
  let remaining_stock = @azimuth.subtract(150, 47)
  @azimuth.assert_eq(103, remaining_stock)  // 剩余库存
  
  let total_stock = @azimuth.add(remaining_stock, 80)
  @azimuth.assert_eq(183, total_stock)        // 新到货后总库存
  
  let boxes_needed = @azimuth.divide_with_ceil(total_stock, 6)
  @azimuth.assert_eq(31, boxes_needed)      // 需要的箱子数量
  
  let remaining_space = @azimuth.subtract(@azimuth.multiply(boxes_needed, 6), total_stock)
  @azimuth.assert_eq(3, remaining_space)    // 剩余空间
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let add1 = @azimuth.add(7, 11)
  let add2 = @azimuth.add(11, 7)
  @azimuth.assert_eq(add1, add2)  // 加法交换律
  
  let mul1 = @azimuth.multiply(7, 11)
  let mul2 = @azimuth.multiply(11, 7)
  @azimuth.assert_eq(mul1, mul2)  // 乘法交换律
  
  let add_assoc1 = @azimuth.add(@azimuth.add(7, 11), 5)
  let add_assoc2 = @azimuth.add(7, @azimuth.add(11, 5))
  @azimuth.assert_eq(add_assoc1, add_assoc2)  // 加法结合律
  
  let mul_assoc1 = @azimuth.multiply(@azimuth.multiply(7, 11), 5)
  let mul_assoc2 = @azimuth.multiply(7, @azimuth.multiply(11, 5))
  @azimuth.assert_eq(mul_assoc1, mul_assoc2)  // 乘法结合律
}