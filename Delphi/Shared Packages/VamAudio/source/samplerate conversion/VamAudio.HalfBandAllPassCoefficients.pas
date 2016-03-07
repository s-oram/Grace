unit VamAudio.HalfBandAllPassCoefficients;

interface

// NOTE: This unit contains coefficients for polyphase half-band filters.
// The coefficients have been copied from Dave Muon's polyphase filter implementation from www.MusicDsp.org
//
// Useful papers for the theory behind these coefficients are:
//
//   "Digital Signal Processing Schemes for Efficient Interpolation and Decimation"
//   - Reinaldo A. Valenzuela, A. G. Constantinides
//
//   "High Performance IIR Filters for Interpolation and Decimation"
//   - Dr David Wheeler.


// NOTE: The steep filters have a sharper transistion band. The less steep filters  have less ripple
// and more overall attenuation.
procedure GetSteepCoeffs(const Order : integer; out CoeffsA, CoeffsB : array of double);
procedure GetLessSteepCoeffs(const Order : integer; out CoeffsA, CoeffsB : array of double);

implementation

uses
  SysUtils;

procedure GetSteepCoeffs(const Order : integer; out CoeffsA, CoeffsB : array of double);
begin

  case Order of
    2: //order=2, rejection=36dB, transition band=0.1
    begin
      // Cascade A...
      CoeffsA[0] := 0.23647102099689224;
      // Cascade B...
      CoeffsB[0]  := 0.7145421497126001;
    end;

    4: //rejection=53dB,transition band=0.05
    begin
      // Cascade A...
      CoeffsA[0] := 0.12073211751675449;
      CoeffsA[1] := 0.6632020224193995;
      // Cascade B...
      CoeffsB[0] := 0.3903621872345006;
      CoeffsB[1] := 0.890786832653497;
    end;

    6: //rejection=51dB, transition band=0.01
    begin
      // Cascade A...
      CoeffsA[0] := 0.1271414136264853;
      CoeffsA[1] := 0.6528245886369117;
      CoeffsA[2] := 0.9176942834328115;

      // Cascade B...
      CoeffsB[0] := 0.40056789819445626;
      CoeffsB[1] := 0.8204163891923343;
      CoeffsB[2] := 0.9763114515836773;
    end;

    8: //rejection=69dB, transition band=0.01
    begin
      // Cascade A...
      CoeffsA[0] := 0.07711507983241622;
      CoeffsA[1] := 0.4820706250610472;
      CoeffsA[2] := 0.7968204713315797;
      CoeffsA[3] := 0.9412514277740471;

      // Cascade B...
      CoeffsB[0] := 0.2659685265210946;
      CoeffsB[1] := 0.6651041532634957;
      CoeffsB[2] := 0.8841015085506159;
      CoeffsB[3] := 0.9820054141886075;
    end;

    10: //rejection=86dB, transition band=0.01
    begin
      // Cascade A...
      CoeffsA[0] := 0.051457617441190984;
      CoeffsA[1] := 0.35978656070567017;
      CoeffsA[2] := 0.6725475931034693;
      CoeffsA[3] := 0.8590884928249939;
      CoeffsA[4] := 0.9540209867860787;

      // Cascade B...
      CoeffsB[0] := 0.18621906251989334;
      CoeffsB[1] := 0.529951372847964;
      CoeffsB[2] := 0.7810257527489514;
      CoeffsB[3] := 0.9141815687605308;
      CoeffsB[4] := 0.985475023014907;
    end;

    12: //rejection=104dB, transition band=0.01
    begin
      // Cascade A...
      CoeffsA[0] := 0.036681502163648017;
      CoeffsA[1] := 0.2746317593794541;
      CoeffsA[2] := 0.56109896978791948;
      CoeffsA[3] := 0.769741833862266;
      CoeffsA[4] := 0.8922608180038789;
      CoeffsA[5] := 0.962094548378084;
      // Cascade B...
      CoeffsB[0] := 0.13654762463195771;
      CoeffsB[1] := 0.42313861743656667;
      CoeffsB[2] := 0.6775400499741616;
      CoeffsB[3] := 0.839889624849638;
      CoeffsB[4] := 0.9315419599631839;
      CoeffsB[5] := 0.9878163707328971;
    end;
  else
    raise Exception.Create('Order count not handled.');
  end;
end;

procedure GetLessSteepCoeffs(const Order : integer; out CoeffsA, CoeffsB : array of double);
begin
  case Order of
    2: //order=2, rejection=36dB, transition band=0.1
    begin
      // Cascade A...
      CoeffsA[0] := 0.23647102099689224;
      // Cascade B...
      CoeffsB[0] := 0.7145421497126001;
    end;

    4: // rejection=70dB,transition band=0.1
    begin
      // Cascade A...
      CoeffsA[0] := 0.07986642623635751;
      CoeffsA[1] := 0.5453536510711322;
      // Cascade B...
      CoeffsB[0] := 0.28382934487410993;
      CoeffsB[1] := 0.8344118914807379;
    end;

    6:  //rejection=80dB, transition band=0.05
    begin
      // Cascade A...
      CoeffsA[0] := 0.06029739095712437;
      CoeffsA[1] := 0.4125907203610563;
      CoeffsA[2] := 0.7727156537429234;

      // Cascade B...
      CoeffsB[0] := 0.21597144456092948;
      CoeffsB[1] := 0.6043586264658363;
      CoeffsB[2] := 0.9238861386532906;
    end;

    8: // rejection=106dB, transition band=0.05
    begin
      // Cascade A...
      CoeffsA[0] := 0.03583278843106211;
      CoeffsA[1] := 0.2720401433964576;
      CoeffsA[2] := 0.5720571972357003;
      CoeffsA[3] := 0.827124761997324;

      // Cascade B...
      CoeffsB[0] := 0.1340901419430669;
      CoeffsB[1] := 0.4243248712718685;
      CoeffsB[2] := 0.7062921421386394;
      CoeffsB[3] := 0.9415030941737551;
    end;

    10: //rejection=133dB, transition band=0.05
    begin
      // Cascade A...
      CoeffsA[0] := 0.02366831419883467;
      CoeffsA[1] := 0.18989476227180174;
      CoeffsA[2] := 0.43157318062118555;
      CoeffsA[3] := 0.6632020224193995;
      CoeffsA[4] := 0.860015542499582;

      // Cascade B...
      CoeffsB[0] := 0.09056555904993387;
      CoeffsB[1] := 0.3078575723749043;
      CoeffsB[2] := 0.5516782402507934;
      CoeffsB[3] := 0.7652146863779808;
      CoeffsB[4] := 0.95247728378667541;
    end;

    12: //rejection=150dB, transition band=0.05
    begin
      // Cascade A...
      CoeffsA[0] := 0.01677466677723562;
      CoeffsA[1] := 0.13902148819717805;
      CoeffsA[2] := 0.3325011117394731;
      CoeffsA[3] := 0.53766105314488;
      CoeffsA[4] := 0.7214184024215805;
      CoeffsA[5] := 0.8821858402078155;
      // Cascade B...
      CoeffsB[0] := 0.06501319274445962;
      CoeffsB[1] := 0.23094129990840923;
      CoeffsB[2] := 0.4364942348420355;
      CoeffsB[3] := 0.6329609551399348;
      CoeffsB[4] := 0.80378086794111226;
      CoeffsB[5] := 0.9599687404800694;
    end;
  else
    raise Exception.Create('Order count not handled.');
  end;
end;

end.
