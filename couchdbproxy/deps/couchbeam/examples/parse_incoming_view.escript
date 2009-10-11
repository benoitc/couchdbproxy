#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin


parse_view_head(Data) ->
    {Data1, end_chunk} = Data,
    try couchbeam_util:split(Data1, "\r\n") of
    [_Head, _, FirstRow] ->
        FirstRow1 = decode_row(FirstRow),
        Head1 = lists:append([binary_to_list(Data1), "]}"]),
        io:format("we just get head and first row ~n", []),
        
        case couchbeam:json_decode(Head1) of
        {[{<<"rows">>, _}]} -> {nil, nil, [FirstRow1], []};
        {[{<<"total_rows">>, TotalRows}, {<<"offset">>, Offset}|_]} ->
            {TotalRows, Offset, [FirstRow1], []}
        end
    catch
        _:_ -> [couchbeam:json_decode(Data)]
    end.
            
decode_row(<<",\r\n",Rest/binary>>) ->
    decode_row(Rest);
decode_row(Row) ->
    couchbeam:json_decode(Row).
    
view_row(Data, Acc) ->
    case Data of
    <<"\r\n]}">> -> Acc;
    <<"\n">> -> Acc;
    _ ->
        io:format("we just get a row ~n", []),
        case Acc of
        {TotalRows, Offset, Rows, Acc1} ->
            [Row] = [Data|Acc1],
            Row1 = decode_row(Row),
            {TotalRows, Offset, [Row1|Rows], []};
        _Acc2 ->
            [Data|Acc]
        end
    end.

end_view(Acc) ->
    io:format("end of view ~n", []),
    case Acc of
    {TotalRows, Offset, Rows, _Rest} ->
        {TotalRows, Offset, lists:reverse(Rows)};
    _Acc2 ->
         iolist_to_binary(lists:reverse([Acc]))
    end.


main(_) ->
    Res = couchbeam:query_view({"benoitc.im", 80}, "b", 
        "blog", "recent-posts", [{"limit", "2"}]),
    io:format("res view ~p ~n", [Res]),
    F = fun(Data, Acc) ->
        case Acc of
        head ->
            parse_view_head(Data);
        _ ->
            case Data of
            {[], done} ->
                end_view(Acc);
            {Data1, end_chunk} ->
                view_row(Data1, Acc);
            _ ->
                case Acc of
                {TotalRows, Offset, Rows, Acc1} ->
                    {TotalRows, Offset, Rows, [Data|Acc1]};
                _ ->
                    [Data|Acc]
                end
            end
        end
    end,
    {raw, Res1} = couchbeam:query_view({"benoitc.im", 80}, "b", 
        "blog", "recent-posts", [{"limit", "2"}], {F, head}),
    io:format("res view ~p ~n", [Res1]),
    ok.