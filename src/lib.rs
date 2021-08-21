use std::fmt::UpperHex;

use num_derive::{FromPrimitive, ToPrimitive};
#[allow(unused_imports)]
use num_traits::{FromPrimitive, ToPrimitive};
use serde::{Serialize, Deserialize};

type BoxError = Box<dyn std::error::Error + Send + Sync>;

pub(crate) mod util;

pub(crate) mod parser {
    use std::convert::Infallible;

    use nom::bytes::complete::*;
    use nom::combinator::*;
    use nom::multi::*;
    use nom::number::complete::*;
    use nom::sequence::*;
    use nom::IResult;

    use super::*;

    mod v0 {
        use super::*;

        fn calculate_texture_size(
            format: TextureFormat,
            type_: TextureType,
            width: usize,
            height: usize,
            mipmap_levels: usize,
        ) -> usize {
            match type_ {
                TextureType::Bitmap => {
                    let mut size = 0;
                    let mut width = width;
                    let mut height = height;
                    let compressed = format.compressed();

                    for _ in 0..mipmap_levels {
                        let mipmap_size = if compressed {
                            ((width + 3) >> 2).max(1)
                                * ((height + 3) >> 2).max(1)
                                * format.block_size()
                        } else {
                            width * height * format.bytes_per_pixel()
                        };

                        width = width >> 1;
                        height = height >> 1;
                        size += mipmap_size;
                    }
                    size
                }
                TextureType::Cubemap => {
                    6 * calculate_texture_size(
                        format,
                        TextureType::Bitmap,
                        width,
                        height,
                        mipmap_levels,
                    )
                }
                TextureType::DepthBuffer => unimplemented!(),
                TextureType::VolumeMap => unimplemented!(),
            }
        }

        fn texture<'a>(input: &'a [u8]) -> IResult<&'a [u8], Texture<'a>> {
            let (
                input,
                (_, format, type_, flags, width, height, mut mipmap_levels, _, _, _, unknown0),
            ) = tuple((
                le_u32,
                map_res(le_u32, |format| {
                    FromPrimitive::from_u32(format).ok_or("Invalid TextureFormat")
                }),
                map_res(le_u32, |type_| {
                    FromPrimitive::from_u32(type_).ok_or("Invalid TextureType")
                }),
                le_u32,
                util::le_u32_as_usize,
                util::le_u32_as_usize,
                util::le_u32_as_usize,
                le_u32,
                le_u32,
                le_u32,
                le_u32,
            ))(input)?;

            if mipmap_levels == 0 {
                let (mut width, mut height) = (width, height);
                while width > 0 && height > 0 {
                    width = width >> 1;
                    height = height >> 1;
                    mipmap_levels += 1;
                }
            }

            let (input, palette) = if format == TextureFormat::R3G3B2P {
                // FIXME: Figure out what no_idea is.
                let (input, no_idea) = le_u16(input)?;
                if no_idea != 0 {
                    let (input, palette) = many_m_n(0x100, 0x100, le_u32)(input)?;
                    (input, Some(palette))
                } else {
                    (input, None)
                }
            } else {
                (input, None)
            };

            let (input, texture) = take(calculate_texture_size(
                format,
                type_,
                width,
                height,
                mipmap_levels,
            ))(input)?;

            Ok((
                input,
                Texture {
                    format,
                    type_,
                    flags,
                    width,
                    height,
                    mipmap_levels,
                    unknown0,
                    palette,
                    texture,
                },
            ))
        }

        fn animation_info(input: &[u8]) -> IResult<&[u8], AnimationInfo> {
            let (
                input,
                (frame_count, start_frame, loop_frame, unknown0, frame_rate, play_mode, playing, _),
            ) = tuple((
                util::le_u32_as_usize,
                le_f32,
                le_f32,
                le_u32,
                le_u32,
                map_res(le_u32, |play_mode| {
                    FromPrimitive::from_u32(play_mode).ok_or("Unsupported PlayMode.")
                }),
                util::le_u8_as_bool,
                take(3usize),
            ))(input)?;

            Ok((
                input,
                AnimationInfo {
                    frame_count,
                    start_frame,
                    loop_frame,
                    unknown0,
                    frame_rate,
                    play_mode,
                    playing,
                },
            ))
        }

        pub fn game_texture<'a>(input: &'a [u8]) -> IResult<&'a [u8], GameTexture<'a>> {
            let (input, _) = le_u32(input)?; // Ignored.
            let (input, _unknown_pointer0) = le_u32(input)?;
            let (input, _status) = le_u32(input)?;
            let (input, path_pointer) = le_u32(input)?;
            let (input, animation_info_pointer) = le_u32(input)?;
            let (input, _unknown) = many_m_n(5, 5, le_u32)(input)?;

            let (input, path) = if path_pointer != 0 {
                let (input, path_length) = le_u16(input)?;
                let (input, path) = map_res(take(path_length), std::str::from_utf8)(input)?;
                (input, Some(&path[..path.len() - 1]))
            } else {
                (input, None)
            };

            let (input, animation_info) = if animation_info_pointer != 0 {
                let (input, animation_info) = animation_info(input)?;
                (input, Some(animation_info))
            } else {
                (input, None)
            };

            let frame_count = if let Some(AnimationInfo { frame_count, .. }) = animation_info {
                frame_count
            } else {
                1
            };

            let (input, textures) = many_m_n(frame_count, frame_count, texture)(input)?;

            Ok((
                input,
                GameTexture {
                    path,
                    animation_info,
                    textures,
                },
            ))
        }
    }

    mod v1 {
        use super::*;

        pub fn game_texture<'a>(input: &'a [u8]) -> IResult<&'a [u8], GameTexture<'a>> {
            let (input, _) = verify(le_u32, |b: &u32| b == &0x31545820)(input)?;
            let (input, size) = util::le_u32_as_usize(input)?;

            let start = input.len();
            let (input, game_texture) = v0::game_texture(input)?;
            assert_eq!(size, start - input.len());

            Ok((input, game_texture))
        }
    }

    fn version(input: &[u8]) -> IResult<&[u8], Version> {
        map_res(
            opt(preceded(verify(le_u16, |b| b == &0xFDFD), le_u16)),
            |version| {
                FromPrimitive::from_u16(version.unwrap_or(0)).ok_or("Unsupported PPAK version.")
            },
        )(input)
    }

    fn game_textures<'a>(
        input: &'a [u8],
        version: Version,
    ) -> IResult<&'a [u8], Vec<GameTexture<'a>>> {
        let (input, count) = util::le_u16_as_usize(input)?;
        log::info!("texture_count = {}", count);

        let game_texture = match version {
            Version::V0 => v0::game_texture,
            Version::V1 => v1::game_texture,
        };
        many_m_n(count, count, game_texture)(input)
    }

    fn languages<'a>(input: &'a [u8], version: Version) -> IResult<&'a [u8], Vec<Language<'a>>> {
        let language = |input: &'a [u8]| -> IResult<&'a [u8], Language<'a>> {
            let (input, _) = verify(le_u16, |b| b == &0xFFFF)(input)?;
            let (input, id) = le_u16(input)?;
            log::info!("language_id = 0x{:02X}", id);
            let (input, block_size) = map_res(le_u32, |size| {
                Result::<usize, Infallible>::Ok(size as usize)
            })(input)?;
            log::trace!("language_size = {}", block_size);

            let block_start = input.len();
            let (input, textures) = game_textures(input, version)?;
            assert_eq!(block_size, block_start - input.len());

            Ok((input, (id, textures)))
        };
        many0(language)(input)
    }

    fn mesh(input: &[u8]) -> IResult<&[u8], (&str, &[u8])> {
        let (input, path_length) = util::le_u16_as_usize(input)?;
        let (input, path) = map_res(take(path_length), std::str::from_utf8)(input)?;
        log::trace!("mesh path = {}", path);
        let (input, _) = le_u16(input)?;
        let (input, size) = util::le_u32_as_usize(input)?;
        log::trace!("mesh size = {}", size);
        let (input, mesh) = take(size)(input)?;
        Ok((input, (&path[..path.len() - 1], mesh)))
    }

    fn meshes(input: &[u8]) -> IResult<&[u8], Vec<(&str, &[u8])>> {
        let (input, _) = le_u32(input)?;
        let (input, mesh_count) = util::le_u16_as_usize(input)?;
        log::info!("mesh_count = {}", mesh_count);
        many_m_n(mesh_count, mesh_count, mesh)(input)
    }

    fn variable(input: &[u8]) -> IResult<&[u8], (&str, &[u8])> {
        let (input, path_length) = util::le_u16_as_usize(input)?;
        let (input, path) = map_res(take(path_length), std::str::from_utf8)(input)?;
        log::trace!("variable path = {}", path);

        let (input, size) = util::le_u32_as_usize(input)?;
        log::trace!("variable size = {}", size);

        let (input, variable) = take(size)(input)?;

        Ok((input, (&path[..path.len() - 1], variable)))
    }

    fn variables(input: &[u8]) -> IResult<&[u8], Vec<(&str, &[u8])>> {
        let (input, variables_count) = util::le_u16_as_usize(input)?;
        log::info!("variables count = {}", variables_count);
        many_m_n(variables_count, variables_count, variable)(input)
    }

    fn script(input: &[u8], version: u16) -> IResult<&[u8], (Option<&str>, &[u8])> {
        let (input, path) = if version == 1 {
            let (input, path_length) = util::le_u16_as_usize(input)?;
            let (input, path) = map_res(take(path_length), std::str::from_utf8)(input)?;
            log::trace!("script path = {}", path);
            (input, Some(&path[..path.len() - 1]))
        } else {
            (input, None)
        };

        let (input, size) = util::le_u32_as_usize(input)?;
        log::trace!("script size = {}", size);

        let (input, script) = take(size)(input)?;

        Ok((input, (path, script)))
    }

    fn scripts(input: &[u8], version: u16) -> IResult<&[u8], Vec<(Option<&str>, &[u8])>> {
        let (input, scripts_count) = util::le_u16_as_usize(input)?;
        log::info!("scripts count = {}", scripts_count);
        many_m_n(scripts_count, scripts_count, |input| script(input, version))(input)
    }

    pub fn ppf<'a>(input: &'a [u8]) -> IResult<&'a [u8], Ppf<'a>> {
        let (input, _) = tag("PPAK")(input)?;
        let (input, version) = version(input)?;
        log::info!("version = 0x{:04X}", version);

        let (input, languages) = languages(input, version)?;
        let (input, game_textures) = game_textures(input, version)?;
        let (input, meshes) = meshes(input)?;
        let (input, scripts_version) = map_res(
            opt(preceded(verify(le_u16, |b| b == &0xFCFC), le_u16)),
            |version| Result::<_, Infallible>::Ok(version.unwrap_or(0)),
        )(input)?;
        log::info!("scripts version = 0x{:04X?}", scripts_version);
        let (input, variables) = variables(input)?;
        let (input, scripts) = scripts(input, scripts_version)?;

        log::info!("remaining in ppf (domain/scene) = 0x{:08X}", input.len());
        log::info!(
            "next {} bytes = {:02X?}",
            input.len().min(10),
            &input[..input.len().min(10)]
        );

        Ok((
            input,
            Ppf {
                version,
                languages,
                game_textures,
                meshes,
                scripts_version,
                variables,
                scripts,
            },
        ))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, FromPrimitive, ToPrimitive)]
#[repr(u16)]
pub enum Version {
    V0 = 0,
    V1,
}

impl UpperHex for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let version = ToPrimitive::to_u16(self).unwrap();
        <u16 as UpperHex>::fmt(&version, f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
#[repr(u32)]
pub enum TextureFormat {
    A8R8G8B8 = 0,
    R8G8B8,
    A4R4G4B4,
    A1R5G5B5,
    R5G6B5 = 5,
    A8 = 7,
    DXT1 = 9,
    DXT3,
    DXT5,
    V8U8,
    R3G3B2P = 14,
}

impl TextureFormat {
    pub fn compressed(&self) -> bool {
        *self >= TextureFormat::DXT1 && *self <= TextureFormat::DXT5
    }

    pub fn block_size(&self) -> usize {
        match *self {
            TextureFormat::DXT1 => 8,
            TextureFormat::DXT3 | TextureFormat::DXT5 => 16,
            _ => unimplemented!(),
        }
    }

    pub fn bytes_per_pixel(&self) -> usize {
        match *self {
            TextureFormat::A8R8G8B8 => 4,
            TextureFormat::R8G8B8 => 3,
            TextureFormat::A4R4G4B4
            | TextureFormat::A1R5G5B5
            | TextureFormat::R5G6B5
            | TextureFormat::V8U8 => 2,
            TextureFormat::A8 | TextureFormat::R3G3B2P => 1,
            _ => unimplemented!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, FromPrimitive, ToPrimitive)]
#[repr(u32)]
pub enum TextureType {
    Bitmap = 0,
    Cubemap,
    VolumeMap,
    DepthBuffer,
}

#[derive(Copy, Clone, Debug, FromPrimitive, ToPrimitive, Serialize, Deserialize)]
#[repr(u32)]
pub enum PlayMode {
    Loop = 0,
    LoopOnce,
    LoopTail,
    Oscillate,
    OscillateOnce,
    OscillateOutOnce,
    OscillateBackOnce,
    Stop,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AnimationInfo {
    pub frame_count: usize,
    pub start_frame: f32,
    pub loop_frame: f32,
    pub unknown0: u32,
    pub frame_rate: u32,
    pub play_mode: PlayMode,
    pub playing: bool,
}

#[derive(Debug)]
pub struct Texture<'a> {
    pub format: TextureFormat,
    pub type_: TextureType,
    pub flags: u32,
    pub width: usize,
    pub height: usize,
    pub mipmap_levels: usize,
    pub unknown0: u32,
    pub palette: Option<Vec<u32>>,
    pub texture: &'a [u8],
}

#[derive(Debug)]
pub struct GameTexture<'a> {
    pub path: Option<&'a str>,
    pub animation_info: Option<AnimationInfo>,
    pub textures: Vec<Texture<'a>>,
}

pub type Language<'a> = (u16, Vec<GameTexture<'a>>);

#[derive(Debug)]
pub struct Ppf<'a> {
    pub version: Version,
    pub languages: Vec<Language<'a>>,
    pub game_textures: Vec<GameTexture<'a>>,
    pub meshes: Vec<(&'a str, &'a [u8])>,
    pub scripts_version: u16,
    pub variables: Vec<(&'a str, &'a [u8])>,
    pub scripts: Vec<(Option<&'a str>, &'a [u8])>,
}

impl<'a> Ppf<'a> {
    pub fn from_slice(slice: &'a [u8]) -> Result<(Ppf<'a>, &[u8]), BoxError> {
        parser::ppf(slice).map(|(input, ppf)| (ppf, input))
            .map_err(|err| {
                log::error!("{}", err);
                "Unable to parse ppf from slice.".into()
            })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
