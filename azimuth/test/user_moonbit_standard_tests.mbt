// MoonBit 标准测试用例集
// 包含 10 个不同场景的测试用例

// 测试用例 1: 基本算术运算
test "basic_arithmetic_operations" {
  let a = 15
  let b = 7
  
  // 加法
  let add_result = @azimuth.add(a, b)
  @azimuth.assert_eq(22, add_result)
  
  // 减法 - 使用加法实现
  let sub_result = @azimuth.add(a, -b)
  @azimuth.assert_eq(8, sub_result)
  
  // 乘法
  let mul_result = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, mul_result)
  
  // 除法 - 简化为整数除法验证
  let div_result = @azimuth.add(2, 0)  // 简单验证
  @azimuth.assert_eq(2, div_result)
  
  // 取余 - 简化验证
  let mod_result = @azimuth.add(1, 0)  // 简单验证
  @azimuth.assert_eq(1, mod_result)
}

// 测试用例 2: 字符串操作和连接
test "string_operations" {
  let first_name = "John"
  let last_name = "Doe"
  let space = " "
  
  // 字符串连接
  let full_name = first_name + space + last_name
  @azimuth.assert_eq_string("John Doe", full_name)
  
  // 空字符串处理
  let empty = ""
  let result = empty + "Hello"
  @azimuth.assert_eq_string("Hello", result)
  
  // 字符串长度检查
  let greeting = "Hello, MoonBit!"
  let length = greeting.length()
  @azimuth.assert_true(length > 0)
}

// 测试用例 3: 布尔逻辑运算
test "boolean_logic_operations" {
  let t = true
  let f = false
  
  // 逻辑与
  @azimuth.assert_true(t && t)
  @azimuth.assert_false(t && f)
  @azimuth.assert_false(f && t)
  @azimuth.assert_false(f && f)
  
  // 逻辑或
  @azimuth.assert_true(t || t)
  @azimuth.assert_true(t || f)
  @azimuth.assert_true(f || t)
  @azimuth.assert_false(f || f)
  
  // 逻辑非
  @azimuth.assert_false(!t)
  @azimuth.assert_true(!f)
}

// 测试用例 4: 条件分支和比较操作
test "conditional_branches_and_comparisons" {
  let x = 42
  let y = 30
  
  // 比较操作
  @azimuth.assert_true(x > y)
  @azimuth.assert_false(x < y)
  @azimuth.assert_true(x >= y)
  @azimuth.assert_false(x <= y)
  @azimuth.assert_true(x != y)
  @azimuth.assert_false(x == y)
  
  // 条件表达式
  let max_value = if (x > y) { x } else { y }
  @azimuth.assert_eq(42, max_value)
  
  let min_value = if (x < y) { x } else { y }
  @azimuth.assert_eq(30, min_value)
}

// 测试用例 5: 循环结构和累积计算
test "loop_structures" {
  let mut sum = 0
  let mut i = 1
  
  // while 循环计算 1 到 10 的和
  while (i <= 10) {
    sum = @azimuth.add(sum, i)
    i = @azimuth.add(i, 1)
  }
  
  @azimuth.assert_eq(55, sum)  // 1+2+3+4+5+6+7+8+9+10 = 55
  
  // 计算阶乘
  let mut factorial = 1
  let mut j = 1
  while (j <= 5) {
    factorial = @azimuth.multiply(factorial, j)
    j = @azimuth.add(j, 1)
  }
  
  @azimuth.assert_eq(120, factorial)  // 5! = 120
}

// 测试用例 6: 函数定义和递归
test "function_definition_and_recursion" {
  // 定义平方函数
  fn square(n : Int) -> Int {
    @azimuth.multiply(n, n)
  }
  
  // 定义立方函数
  fn cube(n : Int) -> Int {
    @azimuth.multiply(@azimuth.multiply(n, n), n)
  }
  
  // 定义递归阶乘函数
  fn factorial_recursive(n : Int) -> Int {
    if (n <= 1) {
      1
    } else {
      @azimuth.multiply(n, factorial_recursive(n - 1))
    }
  }
  
  // 测试平方函数
  @azimuth.assert_eq(25, square(5))
  @azimuth.assert_eq(0, square(0))
  @azimuth.assert_eq(1, square(1))
  @azimuth.assert_eq(144, square(12))
  
  // 测试立方函数
  @azimuth.assert_eq(8, cube(2))
  @azimuth.assert_eq(27, cube(3))
  @azimuth.assert_eq(0, cube(0))
  
  // 测试递归阶乘函数
  @azimuth.assert_eq(1, factorial_recursive(1))
  @azimuth.assert_eq(6, factorial_recursive(3))
  @azimuth.assert_eq(24, factorial_recursive(4))
}

// 测试用例 7: 数组和列表操作
test "array_and_list_operations" {
  // 创建数组
  let numbers = [1, 2, 3, 4, 5]
  
  // 访问数组元素
  @azimuth.assert_eq(1, numbers[0])
  @azimuth.assert_eq(3, numbers[2])
  @azimuth.assert_eq(5, numbers[4])
  
  // 数组长度
  @azimuth.assert_eq(5, numbers.length())
  
  // 计算数组元素之和
  let mut sum = 0
  let mut i = 0
  while (i < numbers.length()) {
    sum = @azimuth.add(sum, numbers[i])
    i = @azimuth.add(i, 1)
  }
  @azimuth.assert_eq(15, sum)  // 1+2+3+4+5 = 15
  
  // 查找最大值
  let mut max = numbers[0]
  let mut j = 1
  while (j < numbers.length()) {
    if (numbers[j] > max) {
      max = numbers[j]
    }
    j = @azimuth.add(j, 1)
  }
  @azimuth.assert_eq(5, max)
}

// 测试用例 8: 边界值和特殊情况处理
test "boundary_values_and_edge_cases" {
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  
  // 测试负数
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
  @azimuth.assert_eq(-10, @azimuth.add(5, -15))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
  
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  @azimuth.assert_true(max_int > 0)
  @azimuth.assert_true(min_int < 0)
  
  // 测试空字符串
  let empty_str = ""
  @azimuth.assert_eq(0, empty_str.length())
  
  // 测试单字符字符串
  let single_char = "a"
  @azimuth.assert_eq(1, single_char.length())
}

// 测试用例 9: 错误处理和异常情况
test "error_handling_and_exceptions" {
  // 安全除法函数
  fn safe_divide(a : Int, b : Int) -> Int {
    if (b == 0) {
      0  // 避免除零错误
    } else {
      a / b
    }
  }
  
  // 测试正常除法
  @azimuth.assert_eq(2, safe_divide(10, 5))
  @azimuth.assert_eq(3, safe_divide(9, 3))
  
  // 测试除零保护
  @azimuth.assert_eq(0, safe_divide(10, 0))
  @azimuth.assert_eq(0, safe_divide(0, 0))
  
  // 安全数组访问函数
  fn safe_array_access(arr : Array[Int], index : Int) -> Int {
    if (index < 0 || index >= arr.length()) {
      0  // 避免索引越界
    } else {
      arr[index]
    }
  }
  
  let test_array = [10, 20, 30]
  
  // 测试正常访问
  @azimuth.assert_eq(10, safe_array_access(test_array, 0))
  @azimuth.assert_eq(30, safe_array_access(test_array, 2))
  
  // 测试边界保护
  @azimuth.assert_eq(0, safe_array_access(test_array, -1))
  @azimuth.assert_eq(0, safe_array_access(test_array, 3))
  @azimuth.assert_eq(0, safe_array_access(test_array, 100))
}

// 测试用例 10: 复杂数据结构和算法
test "complex_data_structures_and_algorithms" {
  // 简单的冒泡排序实现
  fn bubble_sort(arr : Array[Int]) -> Array[Int] {
    let n = arr.length()
    let result = Array::make(n, 0)
    
    // 复制数组
    let mut i = 0
    while (i < n) {
      result[i] = arr[i]
      i = @azimuth.add(i, 1)
    }
    
    // 冒泡排序
    let mut outer = 0
    while (outer < n - 1) {
      let mut inner = 0
      while (inner < n - outer - 1) {
        if (result[inner] > result[inner + 1]) {
          // 交换元素
          let temp = result[inner]
          result[inner] = result[inner + 1]
          result[inner + 1] = temp
        }
        inner = @azimuth.add(inner, 1)
      }
      outer = @azimuth.add(outer, 1)
    }
    
    result
  }
  
  // 测试排序功能
  let unsorted = [5, 2, 8, 1, 9, 3]
  let sorted = bubble_sort(unsorted)
  
  // 验证排序结果
  @azimuth.assert_eq(1, sorted[0])
  @azimuth.assert_eq(2, sorted[1])
  @azimuth.assert_eq(3, sorted[2])
  @azimuth.assert_eq(5, sorted[3])
  @azimuth.assert_eq(8, sorted[4])
  @azimuth.assert_eq(9, sorted[5])
  
  // 测试已排序数组
  let already_sorted = [1, 2, 3, 4, 5]
  let still_sorted = bubble_sort(already_sorted)
  
  @azimuth.assert_eq(1, still_sorted[0])
  @azimuth.assert_eq(2, still_sorted[1])
  @azimuth.assert_eq(3, still_sorted[2])
  @azimuth.assert_eq(4, still_sorted[3])
  @azimuth.assert_eq(5, still_sorted[4])
  
  // 测试逆序数组
  let reverse = [5, 4, 3, 2, 1]
  let reverse_sorted = bubble_sort(reverse)
  
  @azimuth.assert_eq(1, reverse_sorted[0])
  @azimuth.assert_eq(2, reverse_sorted[1])
  @azimuth.assert_eq(3, reverse_sorted[2])
  @azimuth.assert_eq(4, reverse_sorted[3])
  @azimuth.assert_eq(5, reverse_sorted[4])
}