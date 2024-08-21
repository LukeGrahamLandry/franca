function init_pos(x, y)
    return {
        x = x,
        y = y,
        add = function(a, b)
            return init_pos(a.x + b.x, a.y + b.y)
        end,
        sub = function(a, b)
            return init_pos(a.x - b.x, a.y - b.y)
        end,
        mul = function(a, b)
            return init_pos(a.x * b.x, a.y * b.y)
        end,
        div = function(a, b)
            return init_pos(a.x / b.x, a.y / b.y)
        end
    }
end

function mandelbrot(c, z, steps)
    local i = 0
    local zSq = z:mul(z)
    while i < steps and zSq.x + zSq.y < 4.0 do
        z.y = z.x * 2.0 * z.y
        z.x = zSq.x - zSq.y
        z = z:add(c)
        zSq = z:mul(z)
        i = i + 1
    end
    return i
end

function timestamp()
    return os.clock()
end

function main()
    local start = timestamp();
    local max_steps = 45
    local width = 70
    local height = 35
    local x_speed = 0.03
    local y_speed = 0.06
    local x_start = 0.0 - 1.5
    local y_start = 0.0 - 1.0

    local out = ""
    local pos = init_pos(x_start, y_start)
    for h = 1, height do
        for w = 1, width do
            local steps = mandelbrot(pos, init_pos(0.0, 0.0), max_steps)
            if steps == max_steps then
                out = out .. "@"
            else
                out = out .. " "
            end
            pos.x = pos.x + x_speed
        end

        out = out .. "|"
        out = out .. "\n"
        pos.x = x_start
        pos.y = pos.y + y_speed
    end
    print(out)
    local e = timestamp()
    local s = (e - start)
    -- print("Finished running main() in " .. s .. " seconds.")
end

main()
