// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法编写的10个测试用例

// 导入被测试的模块
use azimuth

test "azimuth_add_positive_integers" {
  // 测试正整数加法
  azimuth.assert_eq(8, azimuth.add(3, 5))
  azimuth.assert_eq(100, azimuth.add(40, 60))
  azimuth.assert_eq(0, azimuth.add(0, 0))
}

test "azimuth_add_negative_numbers" {
  // 测试负数加法
  azimuth.assert_eq(-8, azimuth.add(-3, -5))
  azimuth.assert_eq(2, azimuth.add(-3, 5))
  azimuth.assert_eq(-2, azimuth.add(3, -5))
}

test "azimuth_multiply_basic_operations" {
  // 测试基本乘法运算
  azimuth.assert_eq(15, azimuth.multiply(3, 5))
  azimuth.assert_eq(0, azimuth.multiply(0, 100))
  azimuth.assert_eq(-15, azimuth.multiply(-3, 5))
  azimuth.assert_eq(15, azimuth.multiply(-3, -5))
}

test "azimuth_divide_ceil_simple_cases" {
  // 测试向上取整除法的简单情况
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "azimuth_divide_ceil_negative_values" {
  // 测试向上取整除法的负数情况
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(4, azimuth.divide_with_ceil(-10, -3))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(10, -3))
}

test "azimuth_greet_standard_inputs" {
  // 测试问候函数的标准输入
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "azimuth_greet_unicode_characters" {
  // 测试问候函数的Unicode字符处理
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  azimuth.assert_eq_string("Hello, café!", azimuth.greet("café"))
}

test "azimuth_business_calculation_scenario" {
  // 测试业务计算场景
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = azimuth.multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  
  azimuth.assert_eq(40, total_tasks)
  azimuth.assert_eq(3, sprints_needed)
}

test "azimuth_mathematical_commutative_laws" {
  // 测试数学交换律
  let x = 7
  let y = 13
  
  // 加法交换律
  azimuth.assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
}

test "azimuth_complex_calculation_sequence" {
  // 测试复杂计算序列
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = azimuth.multiply(base_value, multiplier)
  let step2 = azimuth.divide_with_ceil(step1, divisor)
  let result = azimuth.add(step2, addition)
  
  azimuth.assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}