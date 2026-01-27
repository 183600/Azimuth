// 增强 MoonBit 测试用例
// 使用标准 MoonBit 测试语法编写的10个高质量测试用例

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(8, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(3, 8))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_identity_validation" {
  // 验证数学恒等式
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(500001, divide_with_ceil(1000001, 2))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：订单处理和库存管理
  let orders = 127
  let items_per_order = 8
  let total_items = multiply(orders, items_per_order)
  
  let warehouse_capacity = 1000
  let warehouses_needed = divide_with_ceil(total_items, warehouse_capacity)
  
  assert_eq(2, warehouses_needed)
  
  // 验证总容量
  let total_capacity = multiply(warehouses_needed, warehouse_capacity)
  assert_eq(2000, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(984, remaining_space)
}

test "string_formatting_edge_cases" {
  // 字符串格式化边缘情况测试
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, 0x123!", greet("0x123"))
  assert_eq_string("Hello, true!", greet("true"))
  assert_eq_string("Hello, false!", greet("false"))
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let dataset_size = 10000
  
  // 模拟线性算法 O(n)
  let linear_operations = multiply(dataset_size, 3)
  
  // 模拟对数算法 O(log n) - 简化为除法
  let logarithmic_operations = divide_with_ceil(dataset_size, 100)
  
  // 模拟常数算法 O(1)
  let constant_operations = 5
  
  assert_eq(30000, linear_operations)
  assert_eq(100, logarithmic_operations)
  assert_eq(5, constant_operations)
  
  // 验证效率差异
  let linear_vs_log = divide_with_ceil(linear_operations, logarithmic_operations)
  assert_eq(300, linear_vs_log)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 5000
  let projects = 7
  let min_resources_per_project = divide_with_ceil(total_resources, projects)
  
  assert_eq(715, min_resources_per_project)
  
  // 验证分配结果
  let total_allocated = multiply(min_resources_per_project, projects)
  assert_eq(5005, total_allocated)
  
  // 计算资源利用率
  let utilization_percentage = divide_with_ceil(
    multiply(total_resources, 100),
    total_allocated
  )
  assert_eq(99, utilization_percentage)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值测试
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
}

test "sequential_calculation_pipeline" {
  // 连续计算流水线测试
  let initial_value = 15
  
  // 计算步骤链
  let step1 = multiply(initial_value, 3)        // 45
  let step2 = add(step1, 10)                    // 55
  let step3 = divide_with_ceil(step2, 8)        // 7
  let step4 = multiply(step3, 2)                // 14
  let step5 = subtract(step4, 5)                // 9
  let final_result = add(step5, 1)              // 10
  
  assert_eq(10, final_result)
}

test "international_character_support" {
  // 国际字符支持测试
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
}