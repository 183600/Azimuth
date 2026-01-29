// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 包含核心功能的全面测试，使用标准 MoonBit 测试语法

// 导入 azimuth 模块
use "../azimuth"

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(azimuth.add(5, 3), azimuth.add(3, 5))
  assert_eq(azimuth.add(10, 15), azimuth.add(15, 10))
  assert_eq(azimuth.add(-2, 8), azimuth.add(8, -2))
  assert_eq(azimuth.add(-5, -3), azimuth.add(-3, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 另一个测试用例
  let x = 2
  let y = 7
  let z = 3
  assert_eq(
    azimuth.multiply(x, azimuth.add(y, z)),
    azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, z))
  )
}

test "divide_with_ceil_comprehensive_cases" {
  // 全面测试向上取整除法函数
  // 正数情况
  assert_eq(4, azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-4, azimuth.divide_with_ceil(-11, 3))  // -3.666... -> -4
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况
  assert_eq(0, azimuth.divide_with_ceil(5, 0))     // 除数为0
  assert_eq(0, azimuth.divide_with_ceil(0, 5))     // 被除数为0
  assert_eq(1, azimuth.divide_with_ceil(5, 5))     // 相等
}

test "greet_function_international_characters" {
  // 测试问候函数的国际字符处理
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", azimuth.greet("ñáéíóú"))
  assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  assert_eq_string("Hello, العربية!", azimuth.greet("العربية"))
  assert_eq_string("Hello, !", azimuth.greet(""))  // 空字符串
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 89
  let quantity = 4
  let shipping_per_item = 5
  let tax_rate = 8  // 8%
  let discount = 20
  
  // 计算小计
  let subtotal = azimuth.multiply(product_price, quantity)
  // 计算运费
  let shipping = azimuth.multiply(shipping_per_item, quantity)
  // 计算税费
  let tax = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = azimuth.add(azimuth.add(subtotal, shipping), tax)
  let final_total = azimuth.add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(20, shipping)
  assert_eq(29, tax)
  assert_eq(405, total_before_discount)
  assert_eq(385, final_total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列求和
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let common_diff = 2
  let num_terms = 10
  
  let sum = azimuth.divide_with_ceil(
    azimuth.multiply(
      num_terms,
      azimuth.add(
        azimuth.multiply(2, first_term),
        azimuth.multiply(num_terms - 1, common_diff)
      )
    ),
    2
  )
  
  assert_eq(120, sum)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "resource_allocation_scenario" {
  // 资源分配场景：项目资源计算
  let total_budget = 10000
  let team_size = 8
  let project_duration_months = 6
  let equipment_cost_per_person = 500
  
  // 计算人力成本（简化）
  let personnel_cost = azimuth.multiply(team_size, azimuth.multiply(1000, project_duration_months))
  // 计算设备成本
  let equipment_cost = azimuth.multiply(equipment_cost_per_person, team_size)
  // 计算总成本
  let total_cost = azimuth.add(personnel_cost, equipment_cost)
  // 计算剩余预算
  let remaining_budget = azimuth.add(total_budget, -total_cost)
  
  assert_eq(48000, personnel_cost)
  assert_eq(4000, equipment_cost)
  assert_eq(52000, total_cost)
  assert_eq(-42000, remaining_budget)  // 预算不足
}

test "time_management_calculation" {
  // 时间管理计算：任务完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = azimuth.divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(6, workdays_needed)  // ceil(45/8) = 6
  assert_eq(2, weeks_needed)     // ceil(6/5) = 2
}

test "data_structure_simulation" {
  // 数据结构模拟：模拟数组操作
  let array_size = 10
  let element_size = 4
  let header_size = 8
  
  // 计算数组总大小
  let data_size = azimuth.multiply(array_size, element_size)
  let total_size = azimuth.add(data_size, header_size)
  
  // 计算可以存储多少个这样的数组在内存中
  let memory_pool = 1024
  let arrays_in_memory = azimuth.divide_with_ceil(memory_pool, total_size)
  
  assert_eq(40, data_size)
  assert_eq(48, total_size)
  assert_eq(22, arrays_in_memory)  // ceil(1024/48) = 22
}