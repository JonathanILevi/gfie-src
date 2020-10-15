/*
    Greenfish Image Converter
    Copyright (c) 2016 B. Szalkai

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgement in the product documentation would be
	   appreciated but is not required.
	2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.
	3. This notice may not be removed or altered from any source distribution.
*/
using System;
using System.Collections.Generic;
using System.Linq;
using Svg;
using System.Drawing;
using System.Drawing.Imaging;
using System.Text;
using System.Runtime.Serialization;
using System.Text.RegularExpressions;
using System.Threading;
using System.Globalization;
using System.IO;

namespace ImageConverter
{
	class Program
	{
		const int TiffTagSoftware = 305;
		const int TiffTagDocumentName = 269;
		const int TiffTagArtist = 315;
		const int TiffTagCopyright = 33432;
		const int TiffTagImageDescription = 270;

		string TryGetStringTagFromTiff(IEnumerable<PropertyItem> propertyItems, int tagId)
		{
			IEnumerable<PropertyItem> piMatches = propertyItems.Where(pi => pi.Id == tagId);
			if (piMatches.Count() == 1)
			{
				PropertyItem pi = piMatches.First();
				return Encoding.UTF8.GetString(pi.Value, 0, pi.Value.Length - 1); // exclude terminating zero
			}
			return "";
		}

		void GetMetadataFromTiff(GfieDocument.Metadata md, Bitmap bm)
		{
			md.Title = TryGetStringTagFromTiff(bm.PropertyItems, TiffTagDocumentName);
			md.Author = TryGetStringTagFromTiff(bm.PropertyItems, TiffTagArtist);
			md.Copyright = TryGetStringTagFromTiff(bm.PropertyItems, TiffTagCopyright);
			md.Comments = TryGetStringTagFromTiff(bm.PropertyItems, TiffTagImageDescription);
			md.Dpi = Math.Min(bm.HorizontalResolution, bm.VerticalResolution);
		}

		PropertyItem NewPropertyItem(int id, short type, byte[] value)
		{
			PropertyItem pi = (PropertyItem)FormatterServices.GetUninitializedObject(typeof(PropertyItem));
			pi.Id = id;
			pi.Type = type;
			pi.Value = value;
			pi.Len = pi.Value.Length;
			return pi;
		}

		const short PropertyTagTypeASCII = 2;

		PropertyItem NewAsciiPropertyItem(int id, string value)
		{
			value = Regex.Replace(value, @"[^\u0000-\u007F]", "?");
			byte[] bytes = new byte[value.Length + 1]; // add terminating zero
			Encoding.ASCII.GetBytes(value, 0, value.Length, bytes, 0);
			bytes[bytes.Length-1] = 0;
			return NewPropertyItem(id, PropertyTagTypeASCII, bytes);
		}

		void SetMetadataToTiff(GfieDocument.Metadata md, Bitmap bm)
		{
			bm.SetPropertyItem(NewAsciiPropertyItem(TiffTagSoftware, "Greenfish Image Converter"));
			bm.SetPropertyItem(NewAsciiPropertyItem(TiffTagDocumentName, md.Title));
			bm.SetPropertyItem(NewAsciiPropertyItem(TiffTagArtist, md.Author));
			bm.SetPropertyItem(NewAsciiPropertyItem(TiffTagCopyright, md.Copyright));
			bm.SetPropertyItem(NewAsciiPropertyItem(TiffTagImageDescription, md.Comments));
			if ((float)md.Dpi > 0)
				bm.SetResolution((float)md.Dpi, (float)md.Dpi);
		}

		static void WriteToConsoleAsWell(StreamWriter sw, string s)
		{
			Console.WriteLine(s);
			sw.WriteLine(s);
		}

		int Run(string[] args)
		{
			Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
			bool silent = (Array.IndexOf(args, "--silent") >= 0);
			try
			{
				if (!silent)
				{
					Console.WriteLine("Greenfish Image Converter");
					Console.WriteLine("Copyright (c) 2016 B. Szalkai");
					Console.WriteLine("Licensed under the zlib license.");
					Console.WriteLine("The dynamically linked library 'Svg.dll' (SVG Rendering Library 2.1.0) is copyrighted by its respective owners and licensed under Microsoft Public License (Ms-PL).");
				}

				if (args.Length < 3)
				{
					if (!silent)
					{
						Console.WriteLine("Usage: ImageConverter <function> <input_file_name> <output_file_name> <params>");
						Console.WriteLine("where <function> can be one of:");
						Console.WriteLine("  * svginfo: no <params> required, a description of the input SVG file will be output");
						Console.WriteLine("  * svg2gfie: <params> must be the scaling parameter (e.g. 1.0)");
						Console.WriteLine("  * tiff2gfie: no <params> required");
						Console.WriteLine("  * gfie2tiff: no <params> required");
					}
					return 1;
				}

				string function = args[0];
				string fnInput = args[1];
				string fnOutput = args[2];

				switch (function.ToLower())
				{
					case "svginfo":
						{
							SvgDocument doc = SvgDocument.Open(fnInput);
							SizeF size = doc.GetDimensions();
							using (StreamWriter sw = new StreamWriter(fnOutput))
							{
								WriteToConsoleAsWell(sw, "width_px=" + size.Width);
								WriteToConsoleAsWell(sw, "height_px=" + size.Height);
								WriteToConsoleAsWell(sw, "dpi=" + doc.Ppi);
							}
						} break;
					case "svg2gfie":
						{
							float scaleFactor = float.Parse(args[3]);
							SvgDocument svg = SvgDocument.Open(fnInput);
							svg.Transforms.Add(new Svg.Transforms.SvgScale(scaleFactor));
							Bitmap bm = new Bitmap((int)Math.Round(svg.Width * scaleFactor),
								(int)Math.Round(svg.Height * scaleFactor), PixelFormat.Format32bppArgb);
							svg.Draw(bm);

							GfieDocument doc = new GfieDocument();
							doc.Pages.Add(new GfieDocument.Page(bm));
							doc.Pages[0].Dpi = Math.Round(svg.Ppi * scaleFactor);

							doc.SaveAsGfie(fnOutput, true);
						} break;
					case "tiff2gfie":
						{
							Bitmap bm = (Bitmap)Image.FromFile(fnInput);
							GfieDocument doc = new GfieDocument();
							GetMetadataFromTiff(doc.Data, bm);
							int pageCount = bm.GetFrameCount(FrameDimension.Page);
							for (int i = 0; i < pageCount; ++i)
							{
								bm.SelectActiveFrame(FrameDimension.Page, i);
								Bitmap bm2 = new Bitmap(bm);
								doc.Pages.Add(new GfieDocument.Page(bm2));
							}
							doc.SaveAsGfie(fnOutput, false);
						} break;
					case "gfie2tiff":
						{
							GfieDocument doc = new GfieDocument();
							doc.LoadAsGfie(fnInput);

							ImageCodecInfo encoderInfo = ImageCodecInfo.GetImageEncoders().First(i => i.MimeType == "image/tiff");
							EncoderParameters encoderParameters = new EncoderParameters(1);
							encoderParameters.Param[0] = new EncoderParameter(System.Drawing.Imaging.Encoder.SaveFlag, (long)EncoderValue.MultiFrame);

							Bitmap bmFirst = null;
							for (int i = 0; i < doc.Pages.Count; ++i)
							{
								GfieDocument.LayerCollection ls = doc.Pages[i].Layers;
								if (ls.Layers.Count != 1
									|| ls.Selection.State == GfieDocument.SelectionState.Floating
									|| ls.Layers[0].BlendMode != GfieDocument.BlendMode.Normal
									|| ls.Layers[0].Image == null
									|| ls.Layers[0].Opacity != 255
									|| !ls.Layers[0].Visible)
								{
									Console.WriteLine("Cannot save layers to tiff");
									return 1;
								}
								Bitmap bmCurrent = ls.Layers[0].Image.Image;
								if (i == 0)
								{
									bmFirst = new Bitmap(bmCurrent);
									SetMetadataToTiff(doc.Data, bmFirst);
									bmFirst.Save(fnOutput, encoderInfo, encoderParameters);
								}
								else
								{
									encoderParameters.Param[0] = new EncoderParameter(System.Drawing.Imaging.Encoder.SaveFlag, (long)EncoderValue.FrameDimensionPage);
									bmFirst.SaveAdd(bmCurrent, encoderParameters);
								}
							}
							encoderParameters.Param[0] = new EncoderParameter(System.Drawing.Imaging.Encoder.SaveFlag, (long)EncoderValue.Flush);
							bmFirst.SaveAdd(encoderParameters);
						} break;
					default:
						if (!silent)
							Console.WriteLine("Unknown function " + function);
						return 2;
				}
				return 0;
			}
			catch (Exception e)
			{
				if (!silent)
					Console.WriteLine(e);
				return 123;
			}
		}

		static int Main(string[] args)
		{
			return new Program().Run(args);
		}
	}
}
