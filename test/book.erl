-module(book).

-export([new/1]).

-record(book, {
      style      :: atom(),
      count      :: integer(),
      available  :: boolean(),
      pages      :: integer(),
      excerpt    :: string(),
      author     :: string(),
      extra      :: [term()]
     }). 

%% the exprecs export of the record interface
-compile({parse_transform, exprecs}).
-export_records([book]).

%% here we provide a mapping of the json key to a record.
new(<<"book">>) ->
    '#new-book'();

%% if the key is unknown, return undefined.
new(_RecName) ->
    undefined.

