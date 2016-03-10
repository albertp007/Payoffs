//
// Payoffs - F# derivatives pricing library
// Copyright (c) 2016 by Albert Pang <albert.pang@me.com> 
// All rights reserved.
//
// This file is a part of Payoffs
//
// Payoffs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Payoffs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
namespace Payoffs

open Payoffs.Data

module YahooFinance = 
  ///
  /// This is a type alias for the static CsvProvider type to use to retrieve
  /// csv output returned from Yahoo Finance historical prices url.
  /// CsvProvider depends on the FSharp.Data.dll assembly
  ///
  type Prices = FSharp.Data.CsvProvider< "Date(date), Open(float), \
      High(float), Low(float), Close(float), Volume(int64), AdjClose(float)" >
  
  /// This is a private binding for a string print formatter to create the url
  /// to retrieve historical prices from Yahoo Finance from a certain start
  /// date
  ///
  let private urlFormatFromDate = 
    sprintf "http://ichart.yahoo.com/table.csv?s=%s&a=%d&b=%d&c=%d&ignore=.csv"
  
  /// This is a private binding for a string print formatter to create the url
  /// to retrive historical prices from Yahoo Finance from the earliest date
  /// available
  ///
  let private urlFormat = 
    sprintf "http://ichart.yahoo.com/table.csv?s=%s&ignore=.csv"
  
  /// <summary>This function creates the URL for downloading historical prices
  /// from Yahoo Finance given the stock code (Reuters code) and the start
  /// date which is of format <c>yyyy-mm-dd</c></summary>
  /// <param name="stockCode">Reuters stock code of the stock</param>
  /// <param name="startDate">String in <c>yyyy-mm-dd</c> format representing
  /// the start date</param>
  /// <returns>the URL as a string</returns>
  ///
  let makeUrl stockCode startDateOption = 
    match startDateOption with
    | None -> urlFormat stockCode
    | Some startDate -> 
      let dt = System.DateTime.Parse startDate
      urlFormatFromDate stockCode (dt.Month - 1) dt.Day dt.Year
  
  /// <summary>This function downloads historical prices using a CsvProvider
  /// with schema representing the csv output from Yahoo Finance</summary>
  /// <param name="startDateOption">String in <c>yyyy-mm-dd</c> format
  /// representing the start date</param>
  /// <param name="stockCode">Reuters code of the stock</param>
  /// <returns>CsvProvider object with the date, open, high, low, close,
  /// volume and adjusted close prices in each row</returns>
  ///
  let downloadHistFromDate startDateOption stockCode = 
    let url = makeUrl stockCode startDateOption
    let prices = Prices.Load url
    prices.Rows
    |> Seq.map (fun p -> 
         { h = p.High
           l = p.Low
           o = p.Open
           c = p.Close
           v = p.Volume
           adj = Some p.AdjClose
           d = p.Date })
    |> Seq.toList
    |> List.rev
  
  // prices.Rows |> Seq.map toBar |> Seq.toList |> List.rev
  /// <summary>This function simply curries downloadHistFromDate with the
  /// optional startDate set to None</summary>
  /// <param name="stockCode">Reuters code of the stock</param>
  /// <returns>CsvProvider object with the date, open, high, low, close,
  /// volume and adjusted close prices in each row</returns>
  ///
  let downloadHist = downloadHistFromDate None
