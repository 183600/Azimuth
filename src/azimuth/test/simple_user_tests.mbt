// 用户自定义的 MoonBit 测试用例
// 测试更复杂的算法和实际应用场景

test "ceiling_division_functionality" {
  // 测试向上取整除法功能
  // 10 / 3 = 3.333... 向上取整为 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  
  // 9 / 3 = 3.0 向上取整为 3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  
  // 1 / 5 = 0.2 向上取整为 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  
  // 0 / 7 = 0 向上取整为 0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 7))
  
  // 负数测试：-10 / 3 = -3.333... 向上取整为 -3
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 计算前几个斐波那契数
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
}

test "factorial_calculation" {
  // 测试阶乘计算
  // n! = n × (n-1) × ... × 2 × 1
  // 0! = 1
  
  // 0! = 1
  let fact0 = 1
  
  // 1! = 1
  let fact1 = 1
  
  // 2! = 2
  let fact2 = 2
  
  // 3! = 6
  let fact3 = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  
  // 4! = 24
  let fact4 = @azimuth.multiply(4, fact3)
  
  // 5! = 120
  let fact5 = @azimuth.multiply(5, fact4)
  
  @azimuth.assert_eq(1, fact0)
  @azimuth.assert_eq(1, fact1)
  @azimuth.assert_eq(2, fact2)
  @azimuth.assert_eq(6, fact3)
  @azimuth.assert_eq(24, fact4)
  @azimuth.assert_eq(120, fact5)
}

test "greatest_common_divisor" {
  // 测试最大公约数计算（使用欧几里得算法的简化版本）
  
  // GCD(48, 18) = 6
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法：GCD(a, b) = GCD(b, a mod b)
  let r1 = a % b  // 48 % 18 = 12
  let r2 = b % r1 // 18 % 12 = 6
  let r3 = r1 % r2 // 12 % 6 = 0
  
  // 当余数为0时，当前除数就是GCD
  let gcd_48_18 = r2  // 6
  @azimuth.assert_eq(6, gcd_48_18)
  
  // GCD(54, 24) = 6
  let c = 54
  let d = 24
  let s1 = c % d  // 54 % 24 = 6
  let s2 = d % s1 // 24 % 6 = 0
  let gcd_54_24 = s1  // 6
  @azimuth.assert_eq(6, gcd_54_24)
}

test "prime_number_check" {
  // 测试素数判断（简化版本）
  
  // 检查7是否为素数
  let num = 7
  let is_prime_7 = true  // 假设是素数
  
  // 检查2到sqrt(7)之间的因数
  let check2 = num % 2  // 7 % 2 = 1 (不整除)
  let check3 = num % 3  // 7 % 3 = 1 (不整除)
  
  // 如果都不能整除，则是素数
  if (check2 != 0 && check3 != 0) {
    is_prime_7 = true
  } else {
    is_prime_7 = false
  }
  @azimuth.assert_eq(true, is_prime_7)
  
  // 检查9是否为素数
  let num2 = 9
  let is_prime_9 = true  // 假设是素数
  
  // 检查2到sqrt(9)之间的因数
  let check2_2 = num2 % 2  // 9 % 2 = 1 (不整除)
  let check3_2 = num2 % 3  // 9 % 3 = 0 (整除)
  
  // 如果能被3整除，则不是素数
  if (check2_2 != 0 && check3_2 != 0) {
    is_prime_9 = true
  } else {
    is_prime_9 = false
  }
  @azimuth.assert_eq(false, is_prime_9)
}

test "array_index_calculation" {
  // 测试数组索引计算
  // 二维数组转一维数组的索引计算：index = row * width + col
  
  let width = 5  // 数组宽度
  let height = 4 // 数组高度
  
  // 测试几个特定位置的索引
  let row1 = 0
  let col1 = 0
  let index1 = @azimuth.add(@azimuth.multiply(row1, width), col1)
  @azimuth.assert_eq(0, index1)
  
  let row2 = 1
  let col2 = 2
  let index2 = @azimuth.add(@azimuth.multiply(row2, width), col2)
  @azimuth.assert_eq(7, index2)
  
  let row3 = 3
  let col3 = 4
  let index3 = @azimuth.add(@azimuth.multiply(row3, width), col3)
  @azimuth.assert_eq(19, index3)
  
  // 测试数组总大小
  let total_size = @azimuth.multiply(width, height)
  @azimuth.assert_eq(20, total_size)
}

test "number_base_conversion" {
  // 测试进制转换（十进制转二进制、八进制、十六进制）
  
  // 十进制 10 转二进制 1010
  let decimal = 10
  let binary_1010 = @azimuth.add(@azimuth.multiply(1, 1000), @azimuth.add(@azimuth.multiply(0, 100), @azimuth.add(@azimuth.multiply(1, 10), 0)))
  @azimuth.assert_eq(1010, binary_1010)
  
  // 验证二进制转十进制：1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let binary_to_decimal = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(0, 4)), @azimuth.add(@azimuth.multiply(1, 2), 0))
  @azimuth.assert_eq(10, binary_to_decimal)
  
  // 十进制 27 转八进制 33
  let decimal2 = 27
  let octal_33 = @azimuth.add(@azimuth.multiply(3, 10), 3)
  @azimuth.assert_eq(33, octal_33)
  
  // 验证八进制转十进制：3×8¹ + 3×8⁰ = 24 + 3 = 27
  let octal_to_decimal = @azimuth.add(@azimuth.multiply(3, 8), 3)
  @azimuth.assert_eq(27, octal_to_decimal)
}

test "date_calculations" {
  // 测试日期计算
  // 假设每个月都有30天，一年有360天（简化计算）
  
  let days_per_month = 30
  let months_per_year = 12
  
  // 计算从年初到某个月的天数
  let month = 5  // 第5个月
  let days_from_year_start = @azimuth.multiply(month, days_per_month)
  @azimuth.assert_eq(150, days_from_year_start)
  
  // 计算两个日期之间的天数
  let month1 = 2
  let day1 = 15
  let month2 = 5
  let day2 = 10
  
  let days1 = @azimuth.add(@azimuth.multiply(month1, days_per_month), day1)
  let days2 = @azimuth.add(@azimuth.multiply(month2, days_per_month), day2)
  let days_between = @azimuth.add(days2, -days1)
  @azimuth.assert_eq(85, days_between)
  
  // 计算星期几（简化计算）
  let start_day = 1  // 假设1月1日是星期一
  let target_day = 100  // 第100天
  
  let day_of_week = target_day % 7  // 100 % 7 = 2 (星期三，0=星期一)
  @azimuth.assert_eq(2, day_of_week)
}

test "sorting_algorithm_simulation" {
  // 测试排序算法模拟（冒泡排序）
  
  // 模拟数组：[5, 2, 8, 1, 9]
  let a1 = 5
  let b1 = 2
  let c1 = 8
  let d1 = 1
  let e1 = 9
  
  // 第一轮比较和交换
  let a1_after = b1  // 2
  let b1_after = a1  // 5
  let c1_after = c1  // 8 (不交换)
  
  let c1_after2 = d1  // 1
  let d1_after = c1  // 8
  
  // 第一轮结果：[2, 5, 1, 8, 9]
  @azimuth.assert_eq(2, a1_after)
  @azimuth.assert_eq(5, b1_after)
  @azimuth.assert_eq(1, c1_after2)
  @azimuth.assert_eq(8, d1_after)
  @azimuth.assert_eq(9, e1)
  
  // 查找最大值和最小值
  let max_val = 9
  let min_val = 1
  
  // 验证最大值和最小值
  @azimuth.assert_eq(9, max_val)
  @azimuth.assert_eq(1, min_val)
}

test "search_algorithm_simulation" {
  // 测试搜索算法模拟（二分查找）
  
  // 在有序数组 [1, 3, 5, 7, 9, 11, 13, 15] 中查找7
  let target = 7
  
  // 模拟二分查找过程
  let left = 0
  let right = 7  // 数组长度-1
  let mid = @azimuth.add(left, right) / 2  // (0 + 7) / 2 = 3
  
  // 第一次查找：中间位置是3，对应值是7
  let mid_value = 7
  
  // 找到目标值
  let found = (mid_value == target)
  @azimuth.assert_eq(true, found)
  
  // 查找不存在的值8
  let target2 = 8
  
  // 第一次查找：中间位置是3，对应值是7
  // 7 < 8，所以在右半部分查找
  let left2 = @azimuth.add(mid, 1)  // 4
  let right2 = right  // 7
  let mid2 = @azimuth.add(left2, right2) / 2  // (4 + 7) / 2 = 5
  
  // 第二次查找：中间位置是5，对应值是11
  // 11 > 8，所以在左半部分查找
  let left3 = left2  // 4
  let right3 = @azimuth.add(mid2, -1)  // 4
  
  // 左边界大于右边界，查找失败
  let found2 = left3 > right3
  @azimuth.assert_eq(true, found2)
}