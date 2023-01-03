def thirdMax(nums):
    nums.sort(reverse = True)
    count = 1
    previous = nums[0]

    for i in range(len(nums)):
        if nums[i] != previous:
            count = count + 1
            previous = nums[i]
        if count == 3:
            return nums[i]
    return nums[0] 


nums = [10, 2, 4, 5, 6, 2, 9, 1, 7, 5, 4, 11, 30]
print(thirdMax(nums))
