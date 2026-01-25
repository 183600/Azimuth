// 标准 MoonBit 测试用例 - 为 azimuth 库添加的高质量测试
// 包含基本功能、边界条件和实际应用场景测试

test "add_function_edge_cases" {
  // 测试加法函数的边缘情况
  @builtin.assert_eq(1000000, add(500000, 500000))
  @builtin.assert_eq(-1000000, add(-500000, -500000))
  @builtin.assert_eq(0, add(-500000, 500000))
  @builtin.assert(2147483647 > add(2147483646, 1))  // 接近最大值
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  @builtin.assert_eq(1000000, multiply(1000, 1000))
  @builtin.assert_eq(-1000000, multiply(-1000, 1000))
  @builtin.assert_eq(1000000, multiply(-1000, -1000))
  @builtin.assert_eq(0, multiply(2147483647, 0))  // 最大值乘零
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  @builtin.assert_eq("Hello, 世界!", greet("世界"))
  @builtin.assert_eq("Hello, café!", greet("café"))
  @builtin.assert_eq("Hello, 🚀!", greet("🚀"))
  @builtin.assert_eq("Hello, ñoño!", greet("ñoño"))
  @builtin.assert_eq("Hello, Москва!", greet("Москва"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  @builtin.assert_eq(1, divide_with_ceil(1, 1000000))  // 极小比例
  @builtin.assert_eq(1000000, divide_with_ceil(1000000, 1))  // 极大比例
  @builtin.assert_eq(2, divide_with_ceil(1000001, 1000000))  // 接近整除
  @builtin.assert_eq(0, divide_with_ceil(-1, 1000000))  // 负小数
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 123
  let y = 456
  
  // 加法交换律
  @builtin.assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  @builtin.assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  @builtin.assert_eq(
    multiply(x, add(y, 10)),
    add(multiply(x, y), multiply(x, 10))
  )
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_products = 1587
  let capacity_per_container = 24
  let containers_needed = divide_with_ceil(total_products, capacity_per_container)
  
  @builtin.assert_eq(67, containers_needed)  // ceil(1587/24) = 67
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, capacity_per_container)
  @builtin.assert_eq(1608, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_products)
  @builtin.assert_eq(21, remaining_capacity)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  @builtin.assert_eq(0, divide_with_ceil(0, 0))  // 0/0
  @builtin.assert_eq(0, divide_with_ceil(2147483647, 0))  // 最大值/0
  @builtin.assert_eq(0, divide_with_ceil(-2147483648, 0))  // 最小值/0
  @builtin.assert_eq(0, divide_with_ceil(-100, 0))  // 负数/0
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 100
  let growth_factor = 3
  let periods = 4
  let divisor = 7
  
  // 复合增长计算
  let growth_result = multiply(initial_value, growth_factor)
  let periodic_result = multiply(growth_result, periods)
  let final_result = divide_with_ceil(periodic_result, divisor)
  
  @builtin.assert_eq(171, final_result)  // ceil(100*3*4/7) = ceil(1200/7) = 171
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}