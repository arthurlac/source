// My two favourite things in life are map and reduce.
function flatten(thingToFlatten, predicate, keyPath) {
    return Object.keys(thingToFlatten)
        .map(key => flattenAux({}, key, thingToFlatten, keyPath, predicate)
        .reduce((acc, itm) => merge(acc, itm), {})
}

function flattenAux(acc, key, base, keyPath, predicate) {
    return !predicate(key, base[key]) ? acc
        : (canflatten(base[key])
            ? flatFilterMap(thing, predicate, genNewKey(key, keyPath))
            : push(acc, key, base[key], keyPath))
}

function push(acc, key, val, keyPath) {
    const newKey = genNewKey(key, keyPath)
    acc[newKey] = val
    return acc
}

function genNewKey(key, keyPath) {
    return keyPath === '' ? key : key+'@'+keyPath
}

function canflatten(x) {
    return x !== null && typeof x === 'object'
}

function merge (a, b) { return Object.assign(a, b) }

module.exports = flatten
