// 标准 MoonBit 测试用例 - Azimuth 项目综合测试
// 包含核心功能的全面测试，使用标准 MoonBit 测试语法

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let a = 15
  let b = 7
  let c = 3
  
  // 测试基本运算
  @azimuth.assert_eq(22, @azimuth.add(a, b))
  @azimuth.assert_eq(105, @azimuth.multiply(a, b))
  @azimuth.assert_eq(8, @azimuth.subtract(a, b))
  
  // 测试复合运算
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(108, result1)  // 15*7 + 3 = 105 + 3 = 108
  
  let result2 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(66, result2)   // (15 + 7) * 3 = 22 * 3 = 66
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数场景
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))
  
  // 负数场景
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "greet_function_comprehensive_features" {
  // 问候函数的综合功能测试
  // 基本功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 特殊字符和 Unicode
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

test "real_world_business_calculations" {
  // 真实世界业务计算测试
  // 场景：计算订单总价
  let unit_price = 45
  let quantity = 12
  let shipping_cost = 25
  let tax_rate = 8
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_cost = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(44, tax_amount)
  @azimuth.assert_eq(609, total_cost)
}

test "inventory_management_calculations" {
  // 库存管理计算测试
  // 场景：计算仓库存储需求
  let item_count = 156
  let box_capacity = 25
  let warehouse_sections = 4
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(item_count, box_capacity)
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = @azimuth.divide_with_ceil(boxes_needed, warehouse_sections)
  
  // 计算剩余容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let remaining_capacity = @azimuth.add(total_capacity, -item_count)
  
  @azimuth.assert_eq(7, boxes_needed)
  @azimuth.assert_eq(2, boxes_per_section)
  @azimuth.assert_eq(175, total_capacity)
  @azimuth.assert_eq(19, remaining_capacity)
}

test "performance_benchmark_calculations" {
  // 性能基准计算测试
  // 场景：计算算法性能指标
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 12
  
  // 计算优化后的操作数
  let optimized_operations = @azimuth.multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = @azimuth.divide_with_ceil(@azimuth.multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = @azimuth.add(optimized_operations, -overhead)
  
  // 计算性能提升倍数
  let performance_improvement = @azimuth.divide_with_ceil(actual_performance, base_operations)
  
  @azimuth.assert_eq(3000, optimized_operations)
  @azimuth.assert_eq(360, overhead)
  @azimuth.assert_eq(2640, actual_performance)
  @azimuth.assert_eq(3, performance_improvement)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  // 场景：计算月度销售统计
  let sales_week1 = 2500
  let sales_week2 = 3200
  let sales_week3 = 2800
  let sales_week4 = 3500
  
  // 计算月度总销售额
  let monthly_total = @azimuth.add(@azimuth.add(@azimuth.add(sales_week1, sales_week2), sales_week3), sales_week4)
  
  // 计算周平均销售额
  let weekly_average = @azimuth.divide_with_ceil(monthly_total, 4)
  
  // 计算增长率（相对于第一周）
  let growth_week2 = @azimuth.divide_with_ceil(@azimuth.multiply(sales_week2 - sales_week1, 100), sales_week1)
  let growth_week4 = @azimuth.divide_with_ceil(@azimuth.multiply(sales_week4 - sales_week1, 100), sales_week1)
  
  @azimuth.assert_eq(12000, monthly_total)
  @azimuth.assert_eq(3000, weekly_average)
  @azimuth.assert_eq(28, growth_week2)
  @azimuth.assert_eq(40, growth_week4)
}

test "complex_string_operations" {
  // 复杂字符串操作测试
  // 场景：构建复杂问候消息
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  let company = "科技公司"
  
  // 构建全名
  let full_name = first_name + last_name
  
  // 构建完整身份
  let full_identity = full_name + " - " + title + " @ " + company
  
  // 生成问候
  let greeting = @azimuth.greet(full_identity)
  
  @azimuth.assert_eq_string("Hello, 张三 - 工程师 @ 科技公司!", greeting)
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(full_name))
}

test "edge_cases_and_error_handling" {
  // 边界情况和错误处理测试
  // 极值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  
  // 大数测试（修复超出范围的问题）
  let large_num = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
  
  // 除法边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(100000, @azimuth.divide_with_ceil(large_num, 1))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, x!", @azimuth.greet("x"))
}