// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能的各种场景

// 导入 lib 模块中的函数
use "azimuth"

test "enhanced_error_handling_division" {
  // 测试除法错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "enhanced_complex_calculations" {
  // 复杂计算测试
  let result1 = add(multiply(3, 4), multiply(5, 6))
  assert_eq(42, result1)  // 3*4 + 5*6 = 12 + 30 = 42
  
  let result2 = divide_with_ceil(multiply(7, 8), 5)
  assert_eq(12, result2)  // ceil(56/5) = 12
}

test "enhanced_string_edge_cases" {
  // 字符串边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "enhanced_mathematical_identities" {
  // 数学恒等式测试
  let x = 15
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零元素性质
}

test "enhanced_negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(-2, add(-7, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "enhanced_real_world_scenario" {
  // 实际应用场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "enhanced_boundary_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "enhanced_large_number_precision" {
  // 大数精度测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(33334, divide_with_ceil(100000, 3))
}

test "enhanced_combinational_logic" {
  // 组合逻辑测试
  let a = 10
  let b = 20
  let c = 30
  
  // 测试 (a + b) * c - a
  let result1 = add(multiply(add(a, b), c), -a)
  assert_eq(890, result1)  // (10 + 20) * 30 - 10 = 30 * 30 - 10 = 900 - 10 = 890
  
  // 测试 a * (b + c) / a
  let result2 = divide_with_ceil(multiply(a, add(b, c)), a)
  assert_eq(50, result2)  // ceil(10 * (20 + 30) / 10) = ceil(500/10) = 50
}

test "enhanced_resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let project1_needs = 30
  let project2_needs = 45
  let project3_needs = 20
  
  let allocated = add(add(project1_needs, project2_needs), project3_needs)
  let remaining = add(total_resources, -allocated)
  let additional_projects = divide_with_ceil(remaining, 5)
  
  assert_eq(95, allocated)
  assert_eq(5, remaining)
  assert_eq(1, additional_projects)
}