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
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;

namespace ImageConverter
{
	class GfieDocument
	{
		public const int MaxWidth = 16384;
		public const int MaxHeight = 16384;

		public class BitmapWithInvertedColor
		{
			public Bitmap Image; // 32-bit base image
			public Bitmap InversionMask; // Can be null. If not null: white: inverted color, black: color from Image.

			public BitmapWithInvertedColor()
			{
			}

			public BitmapWithInvertedColor(Bitmap bm)
			{
				Image = bm;
			}
		}

		public class Metadata
		{
			public string Title;
			public string Author;
			public string Copyright;
			public string Comments;
			public int LoopCount;
			public double Dpi; // Parent DPI. If zero, ICONDOC_DEFAULT_DPI is used.

			public Metadata()
			{
				Clear();
			}

			public void Clear()
			{
				Title = "";
				Author = "";
				Copyright = "";
				Comments = "";
				LoopCount = 0;
				Dpi = 0;
			}
		}

		public enum SelectionState
		{
			None, Selecting, Floating
		}

		public string[] SelectionStateToString = { "none", "selecting", "floating" };

		public class SelectionInfo
		{
			public SelectionState State;
			public BitmapWithInvertedColor Image; // if Floating, 32-bit bitmap
			public Bitmap Mask; // if Selecting, 1-bit bitmap: white is selected, black is not selected
			public Rectangle Box; // if Floating
			public double Angle; // if Floating
			// if (the selection were a layer, it would be the Depth-th in the list
			public int Depth; // if Floating
		}

		public enum BlendMode
		{
			Normal, Mask, Behind, Dissolve,
			Hue, HueShift, Saturation,
			Darken, Multiply, ColorBurn, LinearBurn, DarkerColor,
			Lighten, Screen, ColorDodge, LinearDodge, LighterColor,
			Overlay, SoftLight, HardLight,
			VividLight, LinearLight, PinLight, HardMix,
			Difference, Exclusion
		}

		public string[] BlendModeToString = { "normal", "mask", "behind", "dissolve",
			"hue", "hueShift", "saturation",
			"darken", "multiply", "colorBurn", "linearBurn", "darkerColor",
			"lighten", "screen", "colorDodge", "linearDodge", "lighterColor",
			"overlay", "softLight", "hardLight",
			"vividLight", "linearLight", "pinLight", "hardMix",
			"difference", "exclusion" };

		public class Layer
		{
			public string Name;
			public bool Visible;
			public bool Selected;
			public BitmapWithInvertedColor Image;
			public int Opacity; // 0..255
			public BlendMode BlendMode;

			public Layer()
			{
				Name = "";
				Visible = true;
				Selected = false;
				Image = null;
				Opacity = 255;
				BlendMode = GfieDocument.BlendMode.Normal;
			}

			public Layer(Bitmap bm)
				: this()
			{
				Image = new BitmapWithInvertedColor(bm);
			}
		}

		public class LayerCollection
		{
			public int Width { get; private set; }
			public int Height { get; private set; }
			public List<Layer> Layers;
			public SelectionInfo Selection;

			public LayerCollection()
			{
				Layers = new List<Layer>();
				Selection = new SelectionInfo();
			}

			public LayerCollection(Bitmap bm)
				: this()
			{
				Width = bm.Width;
				Height = bm.Height;
				Layers.Add(new Layer(bm));
				Layers[0].Selected = true;
			}

			public void Resize(int w, int h)
			{
				Width = w;
				Height = h;
			}
		}

		public class Page
		{
			public LayerCollection Layers;
			public Point HotSpot;
			public int FrameRate; // in millisecs
			public double Dpi; // if zero, parent DPI is used

			public Page()
			{
				Layers = new LayerCollection();
			}

			public Page(Bitmap bm)
				: this()
			{
				Layers = new LayerCollection(bm);
			}
		}

		public List<Page> Pages;
		public Metadata Data;

		public GfieDocument()
		{
			Pages = new List<Page>();
			Data = new Metadata();
		}

		public void Clear()
		{
			Pages.Clear();
			Data.Clear();
		}

		public static bool IsGfieFile(Stream s)
		{
			return GfTree.CanLoad(s);
		}

		private static Point ReadPoint(GfTree t)
		{
			Point result = new Point(0, 0);
			if (t.Descend("x")) { result.X = t.CurrentNode.AsInt; t.Ascend(); }
			if (t.Descend("y")) { result.Y = t.CurrentNode.AsInt; t.Ascend(); }
			return result;
		}

		private static Rectangle ReadRect(GfTree t)
		{
			int left, top, right, bottom;
			if (t.Descend("left")) { left = t.CurrentNode.AsInt; t.Ascend(); } else left = 0;
			if (t.Descend("top")) { top = t.CurrentNode.AsInt; t.Ascend(); } else top = 0;
			if (t.Descend("right")) { right = t.CurrentNode.AsInt; t.Ascend(); } else right = 0;
			if (t.Descend("bottom")) { bottom = t.CurrentNode.AsInt; t.Ascend(); } else bottom = 0;
			return new Rectangle(left, top, right - left, bottom - top);
		}

		private static Bitmap ReadRawImage(GfTree t)
		{
			MemoryStream s = new MemoryStream(t.CurrentNode.Data);
			return (Bitmap)Image.FromStream(s);
		}

		private static BitmapWithInvertedColor ReadBitmap(GfTree t)
		{
			if (!t.Descend("format"))
				return null;
			string fmt = t.CurrentNode.AsString.ToUpper();
			t.Ascend();

			if (fmt != "BMP" && fmt != "PNG")
				return null;

			BitmapWithInvertedColor result = new BitmapWithInvertedColor();
			if (!t.Descend("data"))
				return null;
			result.Image = ReadRawImage(t);
			t.Ascend();

			// load inversion mask
			if (t.Descend("inversionMask"))
			{
				result.InversionMask = ReadRawImage(t);
				t.Ascend();
			} // inversion mask
			return result;
		}

		public bool LoadAsGfie(Stream s)
		{
			GfTree t = new GfTree();
			if (!t.Load(s))
				return false;

			Clear();

			try
			{
				// load metadata
				if (t.Descend("metadata"))
				{
					// No need to read jpeg quality
					if (t.Descend("title")) { Data.Title = t.CurrentNode.AsString; t.Ascend(); }
					if (t.Descend("author")) { Data.Author = t.CurrentNode.AsString; t.Ascend(); }
					if (t.Descend("copyright")) { Data.Copyright = t.CurrentNode.AsString; t.Ascend(); }
					if (t.Descend("comments")) { Data.Comments = t.CurrentNode.AsString; t.Ascend(); }
					if (t.Descend("loopCount")) { Data.LoopCount = t.CurrentNode.AsInt; t.Ascend(); }
					if (t.Descend("dpi")) { Data.Dpi = t.CurrentNode.AsDouble; t.Ascend(); }

					t.Ascend();
				}

				// load pages
				if (t.Descend("pages"))
				{
					for (int i = 0; ; ++i)
					{
						if (!t.Descend("page" + i))
							break;

						Page pg = new Page();
						Pages.Add(pg);

						// load layers
						if (t.Descend("layers"))
						{
							LayerCollection ls = pg.Layers;

							// load dimensions
							if (t.Descend("size"))
							{
								Point p = ReadPoint(t);
								ls.Resize(Math.Min(MaxWidth, p.X), Math.Min(MaxHeight, p.Y));
								t.Ascend();
							}

							// load layer objects
							for (int j = 0; ; ++j)
							{
								if (!t.Descend("layer" + j))
									break;

								Layer l = new Layer();
								ls.Layers.Add(l);

								if (t.Descend("name")) { l.Name = t.CurrentNode.AsString; t.Ascend(); }
								if (t.Descend("visible")) { l.Visible = t.CurrentNode.AsBool; t.Ascend(); }
								if (t.Descend("selected")) { l.Selected = t.CurrentNode.AsBool; t.Ascend(); }
								if (t.Descend("image")) { l.Image = ReadBitmap(t); t.Ascend(); }
								if (t.Descend("opacity")) { l.Opacity = t.CurrentNode.AsInt; t.Ascend(); }
								if (t.Descend("blendMode")) { l.BlendMode = (BlendMode)Array.IndexOf(BlendModeToString, t.CurrentNode.AsString); t.Ascend(); }

								t.Ascend();
							}

							// load selection
							if (t.Descend("selection"))
							{
								if (t.Descend("state"))
								{
									ls.Selection.State = (SelectionState)Array.IndexOf(SelectionStateToString, t.CurrentNode.AsString);
									t.Ascend();
								}

								switch (ls.Selection.State)
								{
									case SelectionState.Selecting:
										if (t.Descend("mask"))
										{
											ls.Selection.Mask = ReadBitmap(t).Image;
											t.Ascend();
										}
										break;

									case SelectionState.Floating:
										if (t.Descend("image")) { ls.Selection.Image = ReadBitmap(t); t.Ascend(); }
										if (t.Descend("box")) { ls.Selection.Box = ReadRect(t); t.Ascend(); }
										if (t.Descend("angle")) { ls.Selection.Angle = t.CurrentNode.AsDouble; t.Ascend(); }
										if (t.Descend("depth")) { ls.Selection.Depth = t.CurrentNode.AsInt; t.Ascend(); }
										break;
								}

								t.Ascend();
							}

							t.Ascend();
						}

						// load etc.
						if (t.Descend("hotSpot")) { pg.HotSpot = ReadPoint(t); t.Ascend(); }
						if (t.Descend("frameRate")) { pg.FrameRate = t.CurrentNode.AsInt; t.Ascend(); }
						if (t.Descend("dpi")) { pg.Dpi = t.CurrentNode.AsDouble; t.Ascend(); }

						t.Ascend();
					}

					t.Ascend();
				}
				// success
				return true;
			}
			catch
			{
				return false;
			}
		}

		private static void WritePoint(GfTree t, Point value)
		{
			t.NewChild("x"); t.CurrentNode.AsInt = value.X; t.Ascend();
			t.NewChild("y"); t.CurrentNode.AsInt = value.Y; t.Ascend();
		}

		private static void WriteRect(GfTree t, Rectangle value)
		{
			t.NewChild("left"); t.CurrentNode.AsInt = value.Left; t.Ascend();
			t.NewChild("top"); t.CurrentNode.AsInt = value.Top; t.Ascend();
			t.NewChild("right"); t.CurrentNode.AsInt = value.Right; t.Ascend();
			t.NewChild("bottom"); t.CurrentNode.AsInt = value.Bottom; t.Ascend();
		}

		private static void WriteRawImage(GfTree t, Bitmap bm, bool compressed)
		{
			MemoryStream s = new MemoryStream();
			bm.Save(s, compressed ? ImageFormat.Png : ImageFormat.Bmp);
			t.CurrentNode.Data = s.GetBuffer();
		}

		private static void WriteBitmap(GfTree t, BitmapWithInvertedColor bm, bool compressed)
		{
			// write format
			t.NewChild("format");
			t.CurrentNode.AsString = (compressed ? "png" : "bmp");
			t.Ascend();

			// write image data
			if (bm.Image != null)
			{
				t.NewChild("data");
				WriteRawImage(t, bm.Image, compressed);
				t.Ascend();
			}

			// write inversion mask
			if (bm.InversionMask != null)
			{
				t.NewChild("inversionMask");
				WriteRawImage(t, bm.InversionMask, compressed);
				t.Ascend();
			}
		}

		public void SaveAsGfie(Stream s, bool compressed)
		{
			GfTree t = new GfTree();

			// save metadata
			t.NewChild("metadata");
			if (Data.Title != "") { t.NewChild("title"); t.CurrentNode.AsString = Data.Title; t.Ascend(); }
			if (Data.Author != "") { t.NewChild("author"); t.CurrentNode.AsString = Data.Author; t.Ascend(); }
			if (Data.Copyright != "") { t.NewChild("copyright"); t.CurrentNode.AsString = Data.Copyright; t.Ascend(); }
			if (Data.Comments != "") { t.NewChild("comments"); t.CurrentNode.AsString = Data.Comments; t.Ascend(); }
			t.NewChild("loopCount"); t.CurrentNode.AsInt = Data.LoopCount; t.Ascend();
			if (Data.Dpi > 0) { t.NewChild("dpi"); t.CurrentNode.AsDouble = Data.Dpi; t.Ascend(); }
			t.Ascend();

			// save pages
			t.NewChild("pages");
			for (int i = 0; i < Pages.Count; ++i)
			{
				t.NewChild("page" + i);
				Page page = Pages[i];

				// save layers
				t.NewChild("layers");
				LayerCollection ls = page.Layers;

				// save dimensions
				t.NewChild("size"); WritePoint(t, new Point(ls.Width, ls.Height)); t.Ascend();

				// save layer objects
				for (int j = 0; j < ls.Layers.Count; ++j)
				{
					t.NewChild("layer" + j);
					Layer l = ls.Layers[j];

					t.NewChild("name"); t.CurrentNode.AsString = l.Name; t.Ascend();
					t.NewChild("visible"); t.CurrentNode.AsBool = l.Visible; t.Ascend();
					t.NewChild("selected"); t.CurrentNode.AsBool = l.Selected; t.Ascend();
					t.NewChild("image"); WriteBitmap(t, l.Image, compressed); t.Ascend();
					t.NewChild("opacity"); t.CurrentNode.AsInt = l.Opacity; t.Ascend();
					t.NewChild("blendMode"); t.CurrentNode.AsString = BlendModeToString[(int)l.BlendMode]; t.Ascend();
					t.Ascend(); // layer
				}

				// save selection
				t.NewChild("selection");
				t.NewChild("state"); t.CurrentNode.AsString = SelectionStateToString[(int)ls.Selection.State]; t.Ascend();

				switch (ls.Selection.State)
				{
					case SelectionState.Selecting:
						t.NewChild("mask"); WriteBitmap(t, new BitmapWithInvertedColor(ls.Selection.Mask), compressed); t.Ascend();
						break;

					case SelectionState.Floating:
						t.NewChild("image"); WriteBitmap(t, ls.Selection.Image, compressed); t.Ascend();
						t.NewChild("box"); WriteRect(t, ls.Selection.Box); t.Ascend();
						t.NewChild("angle"); t.CurrentNode.AsDouble = ls.Selection.Angle; t.Ascend();
						t.NewChild("depth"); t.CurrentNode.AsInt = ls.Selection.Depth; t.Ascend();
						break;
				}
				t.Ascend(); // selection
				t.Ascend(); // layers

				t.NewChild("hotSpot"); WritePoint(t, page.HotSpot); t.Ascend();
				t.NewChild("frameRate"); t.CurrentNode.AsInt = page.FrameRate; t.Ascend();
				if (page.Dpi > 0) { t.NewChild("dpi"); t.CurrentNode.AsDouble = page.Dpi; t.Ascend(); }

				t.Ascend(); // pageX
			}

			t.Ascend(); // pages

			t.Save(s);
		}

		public bool LoadAsGfie(string filename)
		{
			using (var fs = new FileStream(filename, FileMode.Open, FileAccess.Read))
				return LoadAsGfie(fs);
		}

		public void SaveAsGfie(string filename, bool compressed)
		{
			using (var fs = new FileStream(filename, FileMode.Create, FileAccess.Write))
				SaveAsGfie(fs, compressed);
		}
	}
}
