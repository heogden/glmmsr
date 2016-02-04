#ifndef GUARD_ITEMSHELPER_H
#define GUARD_ITEMSHELPER_H

#include <algorithm>
#include <vector>

// is A a subset of B?
template <typename T> bool isSubset(std::vector<T>, std::vector<T>);

int findRelativeItem(int, const std::vector<int>&);

std::vector<int> findRelativeItems(const std::vector<int>&,
				   const std::vector<int>&);

std::vector<int> findItemsDifference(const std::vector<int>&,
				     const std::vector<int>&);

std::vector<int> findItemsUnion(const std::vector<int>&,
				const std::vector<int>&);

template <typename T>
bool isSubset(std::vector<T> A, std::vector<T> B)
{
    std::sort(A.begin(), A.end());
    std::sort(B.begin(), B.end());
    return std::includes(B.begin(), B.end(), A.begin(), A.end());
}

#endif // GUARD_ITEMSHELPER_H
