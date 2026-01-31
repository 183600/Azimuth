// 标准 MoonBit 测试用例 - 针对 azimuth 库的10个高质量测试

// 导入被测试的模块
use azimuth

// 测试基本算术运算
test "basic_arithmetic_operations" {
  // 测试加法
  azimuth.assert_eq(7, azimuth.add(3, 4))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-5, azimuth.add(-2, -3))
  
  // 测试乘法
  azimuth.assert_eq(12, azimuth.multiply(3, 4))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(-6, azimuth.multiply(-2, 3))
  
  // 测试减法
  azimuth.assert_eq(2, azimuth.subtract(5, 3))
  azimuth.assert_eq(-2, azimuth.subtract(3, 5))
  azimuth.assert_eq(0, azimuth.subtract(5, 5))
}

// 测试向上取整除法
test "divide_with_ceil_functionality" {
  // 正常除法
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
  
  // 除零处理
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 负数除法
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(-10, -3))
}

// 测试问候函数
test "greet_function_comprehensive" {
  // 基本问候
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  
  // 特殊字符处理
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  
  // 边界情况
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("  "))
}

// 测试数学交换律
test "mathematical_commutative_properties" {
  // 加法交换律
  azimuth.assert_eq(azimuth.add(7, 13), azimuth.add(13, 7))
  azimuth.assert_eq(azimuth.add(-5, 10), azimuth.add(10, -5))
  
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(7, 13), azimuth.multiply(13, 7))
  azimuth.assert_eq(azimuth.multiply(-5, 10), azimuth.multiply(10, -5))
}

// 测试零元素性质
test "zero_element_properties" {
  // 加法零元素
  azimuth.assert_eq(5, azimuth.add(5, 0))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-5, azimuth.add(-5, 0))
  
  // 乘法零元素
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 5))
  azimuth.assert_eq(0, azimuth.multiply(0, 0))
}

// 测试复杂算术表达式
test "complex_arithmetic_expressions" {
  // 组合运算
  let result1 = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  // 嵌套运算
  let result2 = azimuth.multiply(azimuth.add(5, 3), azimuth.subtract(10, 4))
  azimuth.assert_eq(48, result2)  // (5+3)*(10-4) = 8*6 = 48
  
  // 混合运算
  let result3 = azimuth.divide_with_ceil(azimuth.multiply(7, 3), azimuth.add(4, 5))
  azimuth.assert_eq(3, result3)  // ceil(21/9) = 3
}

// 测试业务场景：包装计算
test "business_packaging_scenario" {
  // 计算包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, items_per_box)
  azimuth.assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(5, remaining_space)
}

// 测试边界值和极值
test "boundary_values_and_extremes" {
  // 边界值测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(5, 5))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(3, 2))
  
  // 大数测试
  azimuth.assert_eq(10000, azimuth.multiply(100, 100))
  azimuth.assert_eq(2000000, azimuth.add(1000000, 1000000))
  
  // 极值除法
  azimuth.assert_eq(1000, azimuth.divide_with_ceil(10000, 10))
  azimuth.assert_eq(1001, azimuth.divide_with_ceil(10001, 10))
}

// 测试连续计算流程
test "sequential_calculation_workflow" {
  // 模拟实际工作流程
  let initial_value = 10
  
  // 步骤1：加法
  let step1 = azimuth.add(initial_value, 5)  // 15
  
  // 步骤2：乘法
  let step2 = azimuth.multiply(step1, 2)    // 30
  
  // 步骤3：除法
  let step3 = azimuth.divide_with_ceil(step2, 8)  // 4
  
  // 步骤4：减法
  let final_result = azimuth.subtract(step3, 1)   // 3
  
  azimuth.assert_eq(3, final_result)
}

// 测试错误处理和健壮性
test "error_handling_and_robustness" {
  // 除零错误的多种情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(9999, 0))
  
  // 极值运算
  let max_test = 1000000
  azimuth.assert_eq(0, azimuth.multiply(max_test, 0))
  azimuth.assert_eq(max_test, azimuth.add(max_test, 0))
  
  // 字符串边界情况
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, single!", azimuth.greet("single"))
}