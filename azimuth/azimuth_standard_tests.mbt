// Azimuth 项目标准测试用例
// 包含10个高质量的 MoonBit 测试用例

// 导入被测试的模块
use azimuth

// 测试1: 基本算术运算功能
test "basic_arithmetic_functions" {
  // 测试加法运算
  azimuth.assert_eq(8, azimuth.add(5, 3))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-2, azimuth.add(-5, 3))
  
  // 测试乘法运算
  azimuth.assert_eq(15, azimuth.multiply(5, 3))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(-15, azimuth.multiply(-5, 3))
  
  // 测试减法运算
  azimuth.assert_eq(2, azimuth.subtract(5, 3))
  azimuth.assert_eq(-2, azimuth.subtract(3, 5))
  azimuth.assert_eq(0, azimuth.subtract(5, 5))
}

// 测试2: 向上取整除法的核心功能
test "divide_with_ceil_core_functionality" {
  // 基本除法测试
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
  
  // 除零错误处理
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  
  // 负数除法测试
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(-10, -3))
}

// 测试3: 字符串问候功能
test "greet_function_functionality" {
  // 基本问候测试
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  
  // 特殊字符和Unicode支持
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  
  // 边界情况
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, space!", azimuth.greet("space"))
}

// 测试4: 数学交换律验证
test "mathematical_commutative_laws" {
  // 加法交换律验证
  azimuth.assert_eq(azimuth.add(12, 8), azimuth.add(8, 12))
  azimuth.assert_eq(azimuth.add(-5, 15), azimuth.add(15, -5))
  
  // 乘法交换律验证
  azimuth.assert_eq(azimuth.multiply(12, 8), azimuth.multiply(8, 12))
  azimuth.assert_eq(azimuth.multiply(-5, 15), azimuth.multiply(15, -5))
}

// 测试5: 零元素性质测试
test "zero_element_properties" {
  // 加法零元素性质
  azimuth.assert_eq(7, azimuth.add(7, 0))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-7, azimuth.add(-7, 0))
  
  // 乘法零元素性质
  azimuth.assert_eq(0, azimuth.multiply(7, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 7))
  azimuth.assert_eq(0, azimuth.multiply(0, 0))
}

// 测试6: 复合运算表达式
test "compound_arithmetic_expressions" {
  // 组合运算测试
  let result1 = azimuth.add(azimuth.multiply(3, 4), azimuth.multiply(5, 2))
  azimuth.assert_eq(22, result1)  // 3*4 + 5*2 = 12 + 10 = 22
  
  // 嵌套运算测试
  let result2 = azimuth.multiply(azimuth.add(6, 4), azimuth.subtract(15, 5))
  azimuth.assert_eq(100, result2)  // (6+4)*(15-5) = 10*10 = 100
  
  // 混合运算测试
  let result3 = azimuth.divide_with_ceil(azimuth.multiply(8, 3), azimuth.add(5, 4))
  azimuth.assert_eq(3, result3)  // ceil(24/9) = 3
}

// 测试7: 业务场景 - 库存包装计算
test "business_inventory_packaging" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  azimuth.assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, items_per_box)
  azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(1, remaining_space)
}

// 测试8: 边界值和极值测试
test "boundary_and_extreme_values" {
  // 边界值测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(3, 2))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(99, 100))
  
  // 大数运算测试
  azimuth.assert_eq(1000000, azimuth.multiply(1000, 1000))
  azimuth.assert_eq(2000000, azimuth.add(1000000, 1000000))
  
  // 极值除法测试
  azimuth.assert_eq(1000, azimuth.divide_with_ceil(10000, 10))
  azimuth.assert_eq(1001, azimuth.divide_with_ceil(10001, 10))
}

// 测试9: 连续计算工作流
test "sequential_calculation_workflow" {
  // 模拟实际业务计算流程
  let initial_value = 20
  
  // 步骤1：增加50%
  let step1 = azimuth.add(initial_value, azimuth.divide_with_ceil(initial_value, 2))
  
  // 步骤2：翻倍
  let step2 = azimuth.multiply(step1, 2)
  
  // 步骤3：分配到团队
  let team_size = 7
  let step3 = azimuth.divide_with_ceil(step2, team_size)
  
  // 步骤4：加上缓冲
  let buffer = 3
  let final_result = azimuth.add(step3, buffer)
  
  // 验证最终结果：((20 + 10) * 2) / 7 + 3 = ceil(60/7) + 3 = 9 + 3 = 12
  azimuth.assert_eq(12, final_result)
}

// 测试10: 错误处理和健壮性验证
test "error_handling_and_robustness" {
  // 除零错误全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(99999, 0))
  
  // 极值运算健壮性测试
  let large_number = 1000000
  azimuth.assert_eq(0, azimuth.multiply(large_number, 0))
  azimuth.assert_eq(large_number, azimuth.add(large_number, 0))
  
  // 字符串边界情况健壮性测试
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, x!", azimuth.greet("x"))
  azimuth.assert_eq_string("Hello, 123!", azimuth.greet("123"))
}