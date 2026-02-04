// Azimuth 标准核心测试用例
// 使用标准 MoonBit 测试语法编写的10个高质量测试用例

test "int_absolute_value_functionality" {
  // 测试 Int 模块的绝对值功能
  @azimuth.assert_eq(5, Int::abs(5))
  @azimuth.assert_eq(5, Int::abs(-5))
  @azimuth.assert_eq(0, Int::abs(0))
  @azimuth.assert_eq(2147483647, Int::abs(2147483647))
  @azimuth.assert_eq(2147483647, Int::abs(-2147483647))
}

test "string_basic_operations" {
  // 测试 String 模块的基本操作
  let empty_string = ""
  @azimuth.assert_eq_string("", empty_string)
  
  let hello = "Hello"
  let world = "World"
  let combined = hello + ", " + world + "!"
  @azimuth.assert_eq_string("Hello, World!", combined)
}

test "array_basic_operations" {
  // 测试 Array 模块的基本功能
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  
  @azimuth.assert_eq(3, arr1.length())
  @azimuth.assert_eq(1, arr1[0])
  @azimuth.assert_eq(2, arr1[1])
  @azimuth.assert_eq(3, arr1[2])
  
  // 测试数组连接
  let combined = arr1 + arr2
  @azimuth.assert_eq(6, combined.length())
  @azimuth.assert_eq(1, combined[0])
  @azimuth.assert_eq(6, combined[5])
}

test "azimuth_basic_functions" {
  // 测试 Azimuth 库的基本函数
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-1, @azimuth.subtract(2, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(20, @azimuth.add(7, 13))
  @azimuth.assert_eq(91, @azimuth.multiply(7, 13))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_package = 6
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @azimuth.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 特殊问候情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "mathematical_operations_comprehensive" {
  // 综合数学运算测试
  // 基本算术
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-4, @azimuth.subtract(3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  
  // 零元素性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  // 负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
}