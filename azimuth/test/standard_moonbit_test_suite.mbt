// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，基于现有的 azimuth 模块

// 测试1: 基本算术运算
test "basic_arithmetic_operations" {
  // 测试基本的加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-5, 6))
  
  // 测试基本的乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

// 测试2: 字符串处理
test "string_processing_operations" {
  // 测试字符串拼接
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  
  // 测试字符串长度
  @azimuth.assert_eq(5, "Hello".length())
  @azimuth.assert_eq(0, "".length())
  @azimuth.assert_eq(6, "世界!".length())
}

// 测试3: 向上取整除法
test "divide_with_ceil_operations" {
  // 测试正数向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 测试负数向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

// 测试4: 数学性质验证
test "mathematical_properties" {
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(-5, 10), @azimuth.multiply(10, -5))
  
  // 测试零元素性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

// 测试5: 边界值处理
test "boundary_value_handling" {
  // 测试极值边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

// 测试6: 复杂计算流水线
test "complex_calculation_pipeline" {
  // 测试连续运算
  let step1 = @azimuth.add(10, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2) // 30
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 4
  let step4 = @azimuth.add(step3, 1)      // 5
  
  @azimuth.assert_eq(5, step4)
  
  // 测试嵌套运算
  let result = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.add(2, 4))
  @azimuth.assert_eq(48, result)
}

// 测试7: 业务逻辑场景
test "business_logic_scenarios" {
  // 场景1：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(30, total_capacity)
  
  // 场景2：个性化问候
  let user_name = "张三"
  let greeting = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
}

// 测试8: 数值范围测试
test "numeric_range_operations" {
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000000, 999))
  
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
}

// 测试9: 字符串边界情况
test "string_edge_cases" {
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  
  // 测试长字符串
  let long_name = "VeryLongNameWithManyCharacters"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
}

// 测试10: 综合应用场景
test "comprehensive_application_scenario" {
  // 场景：活动安排计算
  let participants = 87
  let chairs_per_table = 10
  let budget_per_table = 50
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  // 计算剩余座位
  let empty_chairs = @azimuth.add(total_chairs, -participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  // 计算总预算
  let total_budget = @azimuth.multiply(tables_needed, budget_per_table)
  @azimuth.assert_eq(450, total_budget)
  
  // 计算人均预算
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, participants)
  @azimuth.assert_eq(6, budget_per_person)
}