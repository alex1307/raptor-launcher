%%%-------------------------------------------------------------------
%% # Raptor's top level supervisor
%%
%% This module implements the top-level supervisor for the raptors application.
%%%-------------------------------------------------------------------

-module(raptors_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Define children under this supervisor
    RaptorsSrv = {
        raptors_srv,
        {raptors_srv, start_link, []},
        permanent,
        5000,
        worker,
        [raptors_srv]
    },

    %% Crawler Scheduler - периодично стартира crawler services
    %% Интервал: 24 часа = 24 * 60 * 60 * 1000 = 86400000 ms
    CrawlerScheduler = {
        autouncle_crawler_scheduler_sup,  % Уникално ID в supervisor
        {raptor_scheduler, start_link, [autouncle_crawler, 86_400_000]},  % 24 часа
        permanent,
        5000,
        worker,
        [raptor_scheduler]
    },

    RaptorScheduler = {
        raptor_scheduler_sup,  % Уникално ID в supervisor
        {raptor_scheduler, start_link, [raptor, 86_400_000]},  % 24 часа
        permanent,
        5000,
        worker,
        [raptor_scheduler]
    },

    MobileBGScheduler = {
        mobile_bg_scheduler_sup,  % УНИКАЛНО ID! (беше дубликат)
        {raptor_scheduler, start_link, [mobile_bg_crawler, 86_400_000]},  % 24 часа
        permanent,
        5000,
        worker,
        [raptor_scheduler]
    },



    {ok, {{one_for_one, 5, 10}, [
        RaptorsSrv, 
        CrawlerScheduler, 
        MobileBGScheduler,  % Оправено име
        RaptorScheduler]}}.
